{-# LANGUAGE KindSignatures, FlexibleContexts, ScopedTypeVariables, TypeApplications, RankNTypes, FlexibleInstances, ConstraintKinds #-}
module Chopaan.Kibbutz.Kibbutz where

import Prelude hiding (zipWith)
import Streamly
import qualified Streamly.Prelude as S

import Data.Maybe (fromJust, isNothing, isJust)
import Data.Text (Text)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Key

import Control.Applicative (liftA2)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader

import Chopaan.Comm.Comm ( Address
                         , Dispatch
                         , subStream
                         , WriteChan
                         )

import Chopaan.Node.Node ( nodeS
                         , NodeS
                         , registerNodeG
                         , updateNodeG
                         , NodeGauge
                         )
import Chopaan.Node.NodeId ( NodeMAC
                           , NodeId(..)
                           )
import Chopaan.Kibbutz.AWS.Things ( getThings
                                  , thingName
                                  )

import Proto.NodeMessageSchema.NodeMessages ( RuntimeStats
                                            , EnergyState
                                            )
import Chopaan.Utils.StreamsInterop (toEvent)
import qualified Reflex as R
import Reflex.Vty (VtyWidget)
import qualified System.Metrics.Gauge as G
import qualified System.Metrics as EKG
import Data.Int


type KbtzId = Text

newtype Kbtz (t :: (* -> *) -> * -> *) (m :: * -> *) n a = Kbtz {
  unKibbutz :: Map n (t m a)
}

nodes :: Kbtz t m n a -> [n]
nodes = M.keys . unKibbutz

instance (IsStream t, Monad m) => Functor (Kbtz t m n) where
  fmap f (Kbtz m) = Kbtz $ fmap (S.map f) m

instance (Ord n) => Semigroup (Kbtz t m n a) where
  (Kbtz a) <> (Kbtz b) = Kbtz (a <> b)

instance (Ord n) => Monoid (Kbtz t m n a) where
  mempty = Kbtz mempty

instance (IsStream t, MonadAsync m, Ord n, Monoid n) => Applicative (Kbtz t m n) where
  pure a = Kbtz $ M.singleton mempty (S.yield a)
  (Kbtz a) <*> (Kbtz b) = Kbtz $ zipWith (<*>) a b


type KbtzConn t m n a = (IsStream t, MonadAsync m, Address n)

runKbtz :: forall t m n a. KbtzConn t m n a => Kbtz t m n a -> m ()
runKbtz = S.drain . adapt . unify
  where
    unify :: Kbtz t m n a -> t m a
    unify (Kbtz m) = M.foldl' (parallel) (S.nil) m

kbtz
  :: (KbtzConn t m n a)
  => [n]
  -> (n -> m (t m b))
  -> (t m b -> t m a)
  -> m (Kbtz t m n a)
kbtz ns subscribe process = do
  ss <- mapM subscribe ns
  return $ Kbtz . M.fromList $ [(n, process s) | n <- ns, s <- ss]


sensorKbtz :: forall t m. (IsStream t, MonadAsync m)
  => [NodeMAC]
  -> WriteChan NodeMAC EnergyState
  -> m (Kbtz t m NodeMAC NodeS)
sensorKbtz ns q = kbtz ns (sub @t @m @NodeMAC @EnergyState q) nodeS

rsKbtz :: forall t m. (IsStream t, MonadAsync m)
  => [NodeMAC]
  -> WriteChan NodeMAC RuntimeStats
  -> m (Kbtz t m NodeMAC RuntimeStats)
rsKbtz ns q = kbtz ns (sub @t @m @NodeMAC @RuntimeStats q) id

asFRPNetwork :: forall t t' m m' n a.
  (IsStream t, MonadAsync m, R.Reflex t', R.TriggerEvent t' m', MonadIO m', Show a)
  => (forall x. m x -> IO x) -> Kbtz t m n a -> VtyWidget t' m' (Map n (R.Event t' a))
asFRPNetwork h = sequence . (M.map (toEvent @t @t' h)) . unKibbutz

traceKbtz :: (IsStream t, MonadAsync m) => (n -> a -> m ())
          -> Kbtz t m n a
          -> Kbtz t m n a
traceKbtz act (Kbtz k) = Kbtz $ M.mapWithKey (\k stream -> S.trace (act k) stream) k

sub :: forall t m n a. (IsStream t, MonadAsync m, Address n, Dispatch a)
  => WriteChan n a
  -> n
  -> m (t m a)
sub = flip (subStream @t @m @n @a) 

getNodes :: (MonadIO m) => ReaderT KbtzId m [NodeMAC]
getNodes = do
  n <- ask
  ((fmap $ NodeId . fromJust . thingName)
              <$> (liftIO . getThings $ n))

mapStream :: (IsStream t, Monad m, Monad (t m)) => Kbtz t m n a -> t m (Map n a)
mapStream (Kbtz k) = sequence k

taggedS :: forall t m n a. (IsStream t, MonadAsync m) => Kbtz t m n a -> t m (n, a)
taggedS (Kbtz k) = M.foldlWithKey' (nodeTagMerge) (S.fromList []) k
  where
    nodeTagMerge :: t m (n, a) -> n -> t m a -> t m (n, a)
    nodeTagMerge c key s = (S.map (\x -> (key, x)) s) <> c


logNode :: (MonadIO m, Show n, Show a) => n -> a -> m ()
logNode k v = liftIO . print $ "Node: "
                       <> show k
                       <> "\n" <> show v

logKbtz :: (IsStream t, MonadAsync m, Show n, Show a) => Kbtz t m n a -> Kbtz t m n a 
logKbtz = traceKbtz logNode

class Gauged a where
  toInt64 :: a -> Int64
  
--instance Gauged  where
--  toInt64 = registerNodeG

instance Gauged NodeGauge

-- $ Create a store for the kbtz, and NodeGauges for each node, then map the update across
gauge :: forall t m n a b. (IsStream t, MonadAsync m, Ord n, Show n, Gauged b) => EKG.Store -> (EKG.Store -> m (Map n b)) -> (b -> a -> m ()) -> Kbtz t m n a -> m (Kbtz t m n a)
gauge store mkGauge fn kb@(Kbtz km) = do
  gs <- mkGauge store
  return $ traceKbtz (\k s -> fn (gs M.! k) s) kb 

kbtzGauge :: (MonadAsync m, Show n) => Map n (t m NodeS) -> EKG.Store -> m (Map n NodeGauge)
kbtzGauge km store = sequence $ M.mapWithKey (\k _ -> registerNodeG store k) km 

monitor :: (IsStream t, MonadAsync m, Ord n, Show n) => EKG.Store -> Kbtz t m n NodeS -> m (Kbtz t m n NodeS)
monitor store k@(Kbtz km) = gauge store (kbtzGauge km) updateNodeG k >>= (pure . logKbtz)
