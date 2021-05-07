{-# LANGUAGE KindSignatures, FlexibleContexts, ScopedTypeVariables, TypeApplications, RankNTypes, FlexibleInstances, ConstraintKinds, InstanceSigs #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving, GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# LANGUAGE TypeOperators, QuantifiedConstraints, TypeFamilies #-}
module Chopaan.Kibbutz.Kibbutz where

import Prelude hiding (zipWith)


import Streamly.Prelude (IsStream, MonadAsync, parallel)
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL

import GHC.Generics
import Control.DeepSeq (NFData)

import Data.Maybe (fromJust)
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)
import Data.Key

import Data.Bifunctor
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader

import Chopaan.Comm.Comm ( Address
                         , Dispatch
                         , subStream
                         , WriteChan
                         )
       

import Chopaan.Node.NodeId ( NodeMAC
                           , NodeId(..)
                           )

import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.AWS.Things ( getThings
                                  , thingName
                                  , inIotContext
                                  )
import Chopaan.Kibbutz.AWS.Common (newLogger, LogLevel(..))

import ConCat.Scan
import ConCat.Misc


import System.IO

type KbtzConn t m n = (IsStream t, MonadAsync m, Address n)

instance KbtzConn t m n => LScan (Kbtz t m n) where
  lscan :: forall a. (Monoid a) => Kbtz t m n a -> (Kbtz t m n a :* a)
  lscan f = (f, mempty)


newtype Kbtz (t :: (* -> *) -> * -> *) (m :: * -> *) n a = Kbtz { unKibbutz :: Map n (t m a) }
  deriving (Eq, Ord, Show, Generic, Generic1)
  deriving newtype (NFData)

instance (IsStream t, Monad m) => Functor (Kbtz t m n) where
  fmap f (Kbtz m) = Kbtz $ fmap (S.map f) m

instance (Ord n) => Semigroup (Kbtz t m n a) where
  (Kbtz a) <> (Kbtz b) = Kbtz (a <> b)

instance (Ord n) => Monoid (Kbtz t m n a) where
  mempty = Kbtz mempty

instance (IsStream t, MonadAsync m, Ord n, Monoid n) => Applicative (Kbtz t m n) where
  pure a = Kbtz $ M.singleton mempty (pure a)
  (Kbtz f) <*> (Kbtz b) = Kbtz $ zipWith (<*>) f b

instance (IsStream t, Monad m, (forall a. Ord a)) => Bifunctor (Kbtz t m) where
  bimap :: forall n n' a a'. (Ord n')
    => (n -> n')
    -> (a -> a')
    -> Kbtz t m n a
    -> Kbtz t m n' a' 
  bimap f g kbz = Kbtz $ (M.mapKeys f) $ (unKibbutz (g <$> kbz))


streams :: Kbtz t m n a -> [t m a]
streams = (snd <$>) . M.toList . unKibbutz

nodes :: Kbtz t m n a -> [n]
nodes = M.keys . unKibbutz

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

pureFold :: forall m a a'. (Applicative m) => (a' -> a -> a') -> a' -> (a' -> a') -> FL.Fold m a a'
pureFold f i e = FL.Fold (\x y -> pure . FL.Partial $ f x y) (pure i) (pure . e)

stream' :: forall t m n a. (IsStream t, MonadAsync m, Ord n) => Kbtz t m n a -> t m a
stream' = (M.foldl parallel mempty) . unKibbutz

stream :: forall t m n a. (IsStream t, MonadAsync m) => Kbtz t m n a -> t m (n, a)
stream = (M.foldlWithKey taggedParallel mempty) . unKibbutz
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
  liftIO . print $ ("Kbtz Subscribing: " <> show ns)
  ss <- mapM subscribe ns
  return $ Kbtz . M.fromList $ [(n, process s) | n <- ns, s <- ss]

traceKbtz :: (IsStream t, MonadAsync m) => (n -> a -> m ())
          -> Kbtz t m n a
          -> Kbtz t m n a
traceKbtz act (Kbtz k) = Kbtz $ M.mapWithKey (\k' s -> S.trace (act k') s) k

sub :: forall t m n a. (IsStream t, MonadAsync m, Address n, Dispatch a)
  => WriteChan n a
  -> n
  -> m (t m a)
sub = flip (subStream @t @m @n @a) 

getNodes :: (MonadIO m) => KbtzName -> m [NodeMAC]
getNodes (KbtzId n) = do
  lgr <- liftIO $ newLogger Debug stdout
  ((fmap $ NodeId . fromJust . thingName)
              <$> (liftIO . (inIotContext lgr) . getThings $ n))

logNode :: (MonadIO m, Show n, Show a) => n -> a -> m ()
logNode k v = liftIO . print $ "Node: "
                       <> show k
                       <> "\n" <> show v

logKbtz :: (KbtzConn t m n, Show a) => Kbtz t m n a -> Kbtz t m n a 
logKbtz = traceKbtz logNode

