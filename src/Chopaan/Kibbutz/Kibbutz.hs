{-# LANGUAGE KindSignatures, FlexibleContexts, ScopedTypeVariables, TypeApplications, RankNTypes, FlexibleInstances, ConstraintKinds, InstanceSigs, DeriveGeneric, StandaloneDeriving, TypeOperators #-}
module Chopaan.Kibbutz.Kibbutz where

import Prelude hiding (zipWith)
import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL

import GHC.Generics

import Data.Maybe (fromJust, isNothing, isJust)
import Data.Text (Text)
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)
import Data.Key

import Data.Bifunctor
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
import ConCat.Scan
import ConCat.Misc

{--
class Functor f => LScan f where
  lscan :: forall a. Monoid a => f a -> f a :* a
  default lscan :: (Generic1 f, LScan (Rep1 f), Monoid a) => f a -> f a :* a
  lscan = first to1 . lscan . from1
  -- Temporary hack to avoid newtype-like representation. Still needed?
  lscanDummy :: f a
  lscanDummy = undefined
--}



instance KbtzConn t m n => LScan (Kbtz t m n) where
  lscan :: forall a. (Monoid a) => Kbtz t m n a -> (Kbtz t m n a :* a)
  lscan f = (f, mempty)

--deriving instance (IsStream t, MonadAsync m) => Generic1 (t m)

type KbtzId = Text

newtype Kbtz (t :: (* -> *) -> * -> *) (m :: * -> *) n a = Kbtz {
  unKibbutz :: Map n (t m a)
} deriving (Eq, Ord, Show, Generic, Generic1)

nodes :: Kbtz t m n a -> [n]
nodes = M.keys . unKibbutz

instance (IsStream t, Monad m) => Functor (Kbtz t m n) where
  fmap f (Kbtz m) = Kbtz $ fmap (S.map f) m

instance (Ord n) => Semigroup (Kbtz t m n a) where
  (Kbtz a) <> (Kbtz b) = Kbtz (a <> b)

instance (Ord n) => Monoid (Kbtz t m n a) where
  mempty = Kbtz mempty

instance (IsStream t, MonadAsync m, Ord n, Monoid n) => Applicative (Kbtz t m n) where
  pure a = Kbtz $ M.singleton mempty (pure a)
  (Kbtz a) <*> (Kbtz b) = Kbtz $ zipWith (<*>) a b


postscan :: forall t m n a a'. (KbtzConn t m n, Monoid a, Monoid a') => Kbtz t m n a -> (a -> a') -> Kbtz t m n a'
postscan (Kbtz m) f' = Kbtz $ (S.postscan f) <$> m
  where
    f :: FL.Fold m a a'
    f = FL.Fold st in' out
      where
        st :: a' -> a -> m a'
        st a' a = return $ f' a
        in' :: m a'
        in' = (pure mempty)--(pure (pure . const $ mempty) f')
        out :: a' -> m a'
        out = pure

{--
instance (IsStream t, Monad m) => Bifunctor (Kbtz t m) where
  bimap :: forall n a n' a'. (Ord n, Ord n') => (n -> n') -> (a -> a') -> Kbtz t m n a -> Kbtz t m n' a' 
  bimap f g kbtz = Kbtz $ zz
    where
      zz :: Map n' (t m a')
      zz = M.mapKeys f $ yy
      yy :: Map n (t m a')
      yy = unKibbutz xx
      xx :: Kbtz t m n a'
      xx = (g <$> kbtz)
--}

-- The Semantic Function is a scan



type KbtzConn t m n = (IsStream t, MonadAsync m, Address n)

runKbtz :: forall t m n a. KbtzConn t m n => Kbtz t m n a -> t m a
runKbtz = unify
  where
    unify :: Kbtz t m n a -> t m a
    unify = (M.foldl parallel mempty) . unKibbutz

kbtz ::
  forall t m n a b.
  (KbtzConn t m n)
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

logKbtz :: (KbtzConn t m n, Show a) => Kbtz t m n a -> Kbtz t m n a 
logKbtz = traceKbtz logNode

class Gauged a where
  toInt64 :: a -> Int64
  
--instance Gauged  where
--  toInt64 = registerNodeG

instance Gauged NodeGauge

-- $ Create a store for the kbtz, and NodeGauges for each node, then map the update across
gauge :: forall t m n a b. (KbtzConn t m n, Gauged b) => EKG.Store -> (EKG.Store -> m (Map n b)) -> (b -> a -> m ()) -> Kbtz t m n a -> m (Kbtz t m n a)
gauge store mkGauge fn kb@(Kbtz km) = do
  gs <- mkGauge store
  return $ traceKbtz (\k s -> fn (gs M.! k) s) kb 

kbtzGauge :: (MonadAsync m, Show n) => Map n (t m NodeS) -> EKG.Store -> m (Map n NodeGauge)
kbtzGauge km store = sequence $ M.mapWithKey (\k _ -> registerNodeG store k) km 

monitor :: (KbtzConn t m n, Show n) => EKG.Store -> Kbtz t m n NodeS -> m (Kbtz t m n NodeS)
monitor store k@(Kbtz km) = gauge store (kbtzGauge km) updateNodeG k >>= (pure . logKbtz)
