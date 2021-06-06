{-# LANGUAGE KindSignatures, FlexibleContexts, ScopedTypeVariables, TypeApplications, RankNTypes, FlexibleInstances, ConstraintKinds, InstanceSigs #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving, GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# LANGUAGE TypeOperators, QuantifiedConstraints, TypeFamilies #-}
module Chopaan.Kibbutz.Kibbutz where

import Prelude hiding (zipWith)

import Control.Monad
import Streamly.Prelude (IsStream, MonadAsync)
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL

import GHC.Generics

import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)
import Data.Key

import Data.Bifunctor
import Control.Monad.IO.Class (liftIO, MonadIO)

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

type KbtzConn t m n = (IsStream t, MonadAsync m, Ord n, Show n)

instance KbtzConn t m n => LScan (Kbtz t m n) where
  lscan :: forall a. (Monoid a) => Kbtz t m n a -> (Kbtz t m n a :* a)
  lscan f = (f, mempty)


newtype Kbtz (t :: (* -> *) -> * -> *) (m :: * -> *) n a = Kbtz { unKibbutz :: t m (Map n a) }
  deriving (Generic, Generic1)

instance (IsStream t, Monad m) => Functor (Kbtz t m n) where
  fmap f (Kbtz s) = Kbtz $ S.map (fmap f) s

instance (IsStream t, MonadAsync m) => Semigroup (Kbtz t m n a) where
  (Kbtz a) <> (Kbtz b) = Kbtz (a <> b)

instance (IsStream t, MonadAsync m) => Monoid (Kbtz t m n a) where
  mempty = Kbtz mempty

instance (IsStream t, MonadAsync m, Ord n, Monoid n) => Applicative (Kbtz t m n) where
  pure = Kbtz . pure . (M.singleton mempty)
  (Kbtz f) <*> (Kbtz b) = Kbtz $ S.zipWith mAp f b
    where
      mAp :: Map n (a -> b) -> Map n a -> Map n b
      mAp mf m = zipWith (\f' x -> f' x) mf m

instance (IsStream t, Monad m, (forall a. Ord a)) => Bifunctor (Kbtz t m) where
  bimap :: forall n n' a a'. (Ord n')
    => (n -> n')
    -> (a -> a')
    -> Kbtz t m n a
    -> Kbtz t m n' a' 
  bimap f g kbz = Kbtz $ (S.map (M.mapKeys f)) $ (unKibbutz (g <$> kbz))

collK :: (IsStream t, Monad m) => Kbtz t m n a -> t m [(n, a)]
collK = (fmap M.toList) . (unKibbutz)

valuesK :: (IsStream t, Monad m) => Kbtz t m n a -> t m [a]
valuesK = (fmap (fmap snd)) . collK

nodesK :: (IsStream t, Monad m) => Kbtz t m n a -> m ([n])
nodesK = (pure . (fromMaybe [])) <=< ((S.fold FL.head) . S.adapt . (fmap (fmap fst)) . collK)

stream' :: forall t m n a. (IsStream t, MonadAsync m, Ord n) => Kbtz t m n a -> t m a
stream' = (S.concatMapWith S.ahead S.fromList) . valuesK

stream :: forall t m n a. (IsStream t, MonadAsync m) => Kbtz t m n a -> t m (n, a)
stream = (S.concatMapWith S.ahead S.fromList) . collK


kbtz ::
  forall t m n a b.
  (KbtzConn t m n)
  => Map n (FL.Fold m b a)
  -> m (t m (n, b))
  -> m (Kbtz t m n a)
kbtz process getS = (\s -> return . Kbtz $ S.postscan (FL.demux process) s) =<< getS

traceKbtz :: (IsStream t, MonadAsync m) => (Map n a -> m ())
          -> Kbtz t m n a
          -> Kbtz t m n a
traceKbtz act (Kbtz s) = Kbtz $ S.trace (act) s

sub :: forall t m n a. (IsStream t, MonadAsync m, Address n, Dispatch a)
  => WriteChan n a
  -> n
  -> m (t m a)
sub = flip (subStream @t @m @n @a) 

getNodes :: (MonadIO m) => KbtzName -> m [NodeMAC]
getNodes (KbtzId n) = do
  lgr <- liftIO $ newLogger Info stdout
  ((fmap $ NodeId . fromJust . thingName)
              <$> (liftIO . (inIotContext lgr) . getThings $ n))

logNode :: (MonadIO m, Show n, Show a) => n -> a -> m ()
logNode k v = liftIO . print $ "Node: "
                       <> show k
                       <> "\n" <> show v
