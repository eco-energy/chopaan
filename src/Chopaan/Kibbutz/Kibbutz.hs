{-# LANGUAGE KindSignatures, FlexibleContexts, ScopedTypeVariables, TypeApplications, RankNTypes, FlexibleInstances, ConstraintKinds, InstanceSigs, DeriveGeneric, StandaloneDeriving, TypeOperators, QuantifiedConstraints #-}

module Chopaan.Kibbutz.Kibbutz where

import Prelude hiding ((.), id, zipWith, const)


import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL

import GHC.Generics

import Data.Maybe (fromJust)
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
       
import Chopaan.Node.Node ( nodeS, SensorS )
import Chopaan.Node.NodeId ( NodeMAC
                           , NodeId(..)
                           )

import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.AWS.Things ( getThings
                                  , thingName
                                  , inAwsContext
                                  )

import Proto.NodeMessageSchema.NodeMessages ( RuntimeStats
                                            , EnergyState
                                            )

import qualified System.Metrics as EKG

import ConCat.Scan
import ConCat.Misc
import ConCat.Category

import Data.Distributive 


instance KbtzConn t m n => LScan (Kbtz t m n) where
  lscan :: forall a. (Monoid a) => Kbtz t m n a -> (Kbtz t m n a :* a)
  lscan f = (f, mempty)


newtype Kbtz (t :: (* -> *) -> * -> *) (m :: * -> *) n a = Kbtz {
  unKibbutz :: Map n (t m a)
} deriving (Eq, Ord, Show, Generic, Generic1)

streams :: Kbtz t m n a -> [t m a]
streams = (snd <$>) . M.toList . unKibbutz

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


instance (IsStream t, Monad m, (forall a. Ord a)) => Bifunctor (Kbtz t m) where
  bimap :: forall n n' a a'. (Ord n')
    => (n -> n')
    -> (a -> a')
    -> Kbtz t m n a
    -> Kbtz t m n' a' 
  bimap f g kbz = Kbtz $ (M.mapKeys f) $ (unKibbutz (g <$> kbz))


{--
instance (IsStream t, Monad m, (forall n. Monoid n)) => Category (Kbtz t m) where
  id = Kbtz $ M.singleton mempty S.nil
  (.) :: forall b c a. Ok3 (Kbtz t m) a b c => (Kbtz t m b c) -> (Kbtz t m a b) -> (Kbtz t m a c)
  (Kbtz k) . (Kbtz k') = undefined

instance (IsStream t, Monad m) => Distributive (Kbtz t m n) where
  distribute :: Functor f => f (Kbtz t m n a) -> Kbtz t m n (f a)
  distribute kbtz = undefined -- $ streams kbtz 
--}
--instance (IsStream t, Monad m) => Representable (Kbtz t m n)

type KbtzConn t m n = (IsStream t, MonadAsync m, Address n)

-- The Semantic Function is a scan
scanKbtz :: forall t m n a a'. (KbtzConn t m n)
  => Kbtz t m n a
  -> FL.Fold m a a'
  -> Kbtz t m n a'
scanKbtz (Kbtz m) f' = Kbtz $ (S.postscan f') <$> m

scanState :: forall t m n a a'. (KbtzConn t m n, Monad (t m))
  => FL.Fold m (Map n a) (Map n a')
  -> Kbtz t m n a
  -> t m (Map n a')
scanState f = (S.postscan f) . kbtzState

scanfn :: forall t m n a a'. (KbtzConn t m n)
  => Kbtz t m n a
  -> (a' -> a -> a')
  -> a'
  -> Kbtz t m n a'
scanfn k f i = scanKbtz k $ pureFold f i id 

pureFold :: (Applicative m) => (a' -> a -> a') -> a' -> (a' -> a') -> FL.Fold m a a'
pureFold f i e = FL.Fold (\x y -> pure $ f x y) (pure i) (pure . e)

runKbtz :: forall t m n a. (IsStream t, MonadAsync m, Ord n) => Kbtz t m n a -> t m a
runKbtz = (M.foldl parallel mempty) . unKibbutz

runKbtzKeyed :: forall t m n a. (IsStream t, MonadAsync m) => Kbtz t m n a -> t m (n, a)
runKbtzKeyed = (M.foldlWithKey taggedParallel mempty) . unKibbutz
  where
    taggedParallel :: t m (n, a) -> n -> t m a -> t m (n, a)
    taggedParallel c key s = (S.zipWith (,) (S.repeat key) s) `parallel` c

kbtzState :: (IsStream t, Monad m, Monad (t m)) => Kbtz t m n a -> t m (Map n a)
kbtzState (Kbtz k) = sequence k

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
  -> m (Kbtz t m NodeMAC SensorS)
sensorKbtz ns q = kbtz ns (sub @t @m @NodeMAC @EnergyState q) nodeS

rsKbtz :: forall t m. (IsStream t, MonadAsync m)
  => [NodeMAC]
  -> WriteChan NodeMAC RuntimeStats
  -> m (Kbtz t m NodeMAC RuntimeStats)
rsKbtz ns q = kbtz ns (sub @t @m @NodeMAC @RuntimeStats q) id

traceKbtz :: (IsStream t, MonadAsync m) => (n -> a -> m ())
          -> Kbtz t m n a
          -> Kbtz t m n a
traceKbtz act (Kbtz k) = Kbtz $ M.mapWithKey (\k' stream -> S.trace (act k') stream) k

sub :: forall t m n a. (IsStream t, MonadAsync m, Address n, Dispatch a)
  => WriteChan n a
  -> n
  -> m (t m a)
sub = flip (subStream @t @m @n @a) 

getNodes :: (MonadIO m) => ReaderT KbtzName m [NodeMAC]
getNodes = do
  (KbtzId n) <- ask
  ((fmap $ NodeId . fromJust . thingName)
              <$> (liftIO . inAwsContext . getThings $ n))

logNode :: (MonadIO m, Show n, Show a) => n -> a -> m ()
logNode k v = liftIO . print $ "Node: "
                       <> show k
                       <> "\n" <> show v

logKbtz :: (KbtzConn t m n, Show a) => Kbtz t m n a -> Kbtz t m n a 
logKbtz = traceKbtz logNode

{--

class Gauged a
  
--instance Gauged  where
--  toInt64 = registerNodeG

instance Gauged NodeGauge


-- $ Create a store for the kbtz, and NodeGauges for each node, then map the update across
gauge :: forall t m n a b. (KbtzConn t m n, Gauged b) => EKG.Store -> (EKG.Store -> m (Map n b)) -> (b -> a -> m ()) -> Kbtz t m n a -> m (Kbtz t m n a)
gauge store mkGauge fn kb = do
  gs <- mkGauge store
  return $ traceKbtz (\k s -> fn (gs M.! k) s) kb 

kbtzGauge :: (MonadAsync m, Show n) => Map n (t m NodeS) -> EKG.Store -> m (Map n NodeGauge)
kbtzGauge km store = sequence $ M.mapWithKey (\k _ -> registerNodeG store k) km 

monitor :: (KbtzConn t m n, Show n) => EKG.Store -> Kbtz t m n NodeS -> m (Kbtz t m n NodeS)
monitor store k@(Kbtz km) = gauge store (kbtzGauge km) updateNodeG k >>= (pure . logKbtz)
--}
