{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes, ConstraintKinds, KindSignatures, QuantifiedConstraints, MultiParamTypeClasses, GADTs, FlexibleInstances#-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns  #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveAnyClass, DeriveFunctor, StandaloneDeriving, TupleSections, AllowAmbiguousTypes, BangPatterns #-}

module Chopaan.Hydrate ( hydrateKbtz
                       , hydrateKbtz'
                       , hydrateKbtzM
                       , hydrateKbtzM'
                       , Hydration
                       ) where

-- import Control.Arrow
-- import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
-- import Control.Concurrent.STM

import Streamly.Prelude as S (IsStream, MonadAsync, adapt)
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Data.Fold as FL
-- import qualified Streamly.Internal.Data.Fold.Type as FL
-- import qualified Streamly.Internal.Data.Fold.Tee as FL
-- import qualified Streamly.Internal.Data.Unfold as UF
-- import qualified Streamly.Internal.Data.Pipe as P
-- import qualified Streamly.Data.Array.Foreign as A


import Data.Time
--import Data.Maybe
import qualified Data.Map.Lazy as M

import Network.AWS.S3 (s3, ObjectKey, BucketName(..))

import Chopaan.Types (HydrationOpts(..), toUTC, Resolution)
import Proto.NodeMessageSchema.NodeMessages (RuntimeStats, EnergyState)
import Chopaan.Kibbutz.AWS.Common (getAwsEnv, Env)
import Chopaan.Comm.S3
import Chopaan.Node.NodeId
import Chopaan.Node.Folds
import Chopaan.Node.Mesh
import Chopaan.Graph
import Chopaan.Kibbutz


hydrateKbtz' :: forall t . (IsStream t)
             => HydrationOpts
             -> KbtzC NodeMAC
             -> t GraphM Hydration
hydrateKbtz' h = S.concatM . hydrateKbtz h


hydrateKbtzM' :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
             => DBPools
             -> HydrationOpts
             -> KbtzC NodeMAC
             -> t m Hydration
hydrateKbtzM' poo h = S.concatM . (hydrateKbtzM poo h)

hydrateKbtzM :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
             => DBPools
             -> HydrationOpts
             -> KbtzC NodeMAC
             -> m (t m Hydration)
hydrateKbtzM poo h = (pure . S.adapt . S.hoist (runGraphWithDB poo))
                   <=< (runGraphWithDB poo . hydrateKbtz h)

--type Hydration' = (NodeMAC, Either SensorR (MeshNode, RxSignal))

type Hydration = (NodeMAC, Bool)

type Range = (UTCTime, UTCTime)


-- concatIxFoldableWith :: forall t m n a.
--   (Ord n, IsStream t, MonadAsync m) => (forall b. t m b -> t m b -> t m b) -> M.Map n (t m a) -> t m (n, a)
-- concatIxFoldableWith conc = M.foldrWithKey c S.nil
--   where
--     c :: n -> t m a -> t m (n, a) -> t m (n, a)
--     c k v acc = (fmap (k,) v) `conc` acc 
-- {-# INLINE concatIxFoldableWith #-}


concatMapIxFoldableWith :: forall t m n a x.
  (Ord n, IsStream t, MonadAsync m)
  => (forall b. t m b -> t m b -> t m b)
  -> (n -> x -> t m a)
  -> M.Map n x
  -> t m (n, a)
concatMapIxFoldableWith conc f = M.foldrWithKey c S.nil
  where
    c :: n -> x -> t m (n, a) -> t m (n, a)
    c k v acc = (fmap (k,) (f k v)) `conc` acc 
{-# INLINE concatMapIxFoldableWith #-}

hydrateKbtz :: forall t. (IsStream t) => HydrationOpts -> KbtzC NodeMAC -> GraphM (t GraphM Hydration)
hydrateKbtz HydrationOpts{dbSave, start, end, s3BucketName, resolution, bufOpts} KbtzC{name, nodes} =  (\env -> pure $ hydrationRate S.|$ concatMapIxFoldableWith S.parallel (nodeS env) lsyncs)
  =<< (getAwsEnv s3)
    where
      hydrationRate = S.tapRate 10 (liftIO . (print . (prefix <>) . show))
      lsyncs = M.fromList $ zip nodes (repeat Nothing)
      nodeS :: Env -> NodeMAC -> Maybe ObjectKey -> t GraphM (Bool)
      nodeS env n ls = grider n
                   S.|$ mesher n
                   S.|$ S.fromAhead
                   $ nodeS3 env buck resolution bufOpts (toUTC start, toUTC end) n ls
      buck = (BucketName s3BucketName)
      mesher :: NodeMAC -> (t GraphM (Either EnergyState RuntimeStats))
        -> t GraphM (Either EnergyState (MeshNode, RxSignal))
      mesher n s = S.trace (getSaveM)
        S.|$ S.mapM (pure . mfn)
        S.|$ s               
        where
          getSaveM = case dbSave of
            True -> saveM
            False -> pure . (const True)
          mfn :: (Either x RuntimeStats)
            -> (Either x (MeshNode, RxSignal)) 
          mfn = (fmap ((meshNodeLink (getGridRoot name))))
          saveM :: (Either a (MeshNode, RxSignal)) -> GraphM Bool
          saveM x = case x of
            (Left _) -> return True
            (Right !r) -> do
              withSpider $! pE =<< addMeshN (n, r)
      grider :: NodeMAC -> t GraphM (Either EnergyState (MeshNode, RxSignal))
                 -> t GraphM Bool
      grider n s = S.mapM (withSpider . (getSaveG))
                   S.|$ S.postscan sensorFold
                   S.|$ S.lefts s
        where
          getSaveG = case dbSave of
            True -> saveGrid
            False -> pure . (const True)
          saveGrid !x = do
            !a <- pE =<< addFlow name (n, x)
            !b <- pE =<< addMon name (n, (x, Nothing))
            !c <- pE =<< addTx name (n, (x, Nothing, Nothing))
            return (a && b && c)

      prefix = "Hydration Rate: "
      pE :: Either SpiderException Bool -> SpiderM Bool
      pE r = case r of
        (Left e) -> (liftIO . print $ e) >> return False
        (Right a) -> return a
          -- saveGM _ n (Right r) = addMeshN (n, r)
          -- saveGM k n (Left s) = saveGrid k n s
          -- flR :: (Monad m) => FL.Fold m a c -> FL.Fold m (Either a b) (c, b)  
          -- flR rf = FL.partition rf idFold
