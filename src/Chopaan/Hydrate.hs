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



s3Stream' :: (IsStream t, MonadAsync m, MonadCatch m)
  => Env
  -> S3Opts
  -> Resolution
  -> [(NodeMAC, Maybe ObjectKey)]
  -> (UTCTime, UTCTime)
  -> M.Map NodeMAC (t m (Either EnergyState RuntimeStats))
s3Stream' env b res ns range = M.fromList $ fmap (\(n, o) -> (n, S.fromAhead $ nodeS3 env b res range n o)) ns
{-# INLINE s3Stream' #-}

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

concatIxFoldableWith :: forall t m n a.
  (Ord n, IsStream t, MonadAsync m) => (forall b. t m b -> t m b -> t m b) -> M.Map n (t m a) -> t m (n, a)
concatIxFoldableWith conc = M.foldrWithKey c S.nil
  where
    c :: n -> t m a -> t m (n, a) -> t m (n, a)
    c k v acc = (fmap (k,) v) `conc` acc 
{-# INLINE concatIxFoldableWith #-}

hydrateKbtz :: forall t. (IsStream t) => HydrationOpts -> KbtzC NodeMAC -> GraphM (t GraphM Hydration)
hydrateKbtz HydrationOpts{dbSave, start, end, s3BucketName, resolution} KbtzC{name, nodes} = do
  env <- getAwsEnv s3
  let
    lsyncs = zip nodes (repeat Nothing)
  return $ S.tapRate 10 (liftIO . (print . (prefix <>) . show))
    $ S.minRate 1000
    $ concatIxFoldableWith S.parallel --IxFoldable
    $ grider
    $ mesher (s3Stream' env (BucketName s3BucketName) resolution lsyncs (toUTC start, toUTC end))
    where
      mesher :: M.Map NodeMAC (t GraphM (Either EnergyState RuntimeStats))
        -> M.Map NodeMAC (t GraphM (Either EnergyState (MeshNode, RxSignal)))
      mesher  = M.mapWithKey (\n s -> S.trace (getSaveM n) $
                                      S.mapM (pure . mfn) $ s)               
        where
          getSaveM = case dbSave of
            True -> saveM
            False -> pure . pure . (const True)
          mfn :: (Either x RuntimeStats)
            -> (Either x (MeshNode, RxSignal)) 
          mfn = (fmap ((meshNodeLink (getGridRoot name))))
          saveM :: NodeMAC -> (Either a (MeshNode, RxSignal)) -> GraphM Bool
          saveM n x = case x of
            (Left _) -> return True
            (Right r) -> do
              withSpider $! pE =<< addMeshN (n, r)
      grider :: M.Map NodeMAC (t GraphM (Either EnergyState (MeshNode, RxSignal)))
                 -> M.Map NodeMAC (t GraphM Bool)
      grider = M.mapWithKey (\n s -> S.mapM (withSpider . (getSaveG n))
                                     $ S.postscan sensorFold
                                     $ S.lefts s)
        where
          getSaveG = case dbSave of
            True -> saveGrid
            False -> pure . pure . (const True)
      prefix = "Hydration Rate: "
      saveGrid n = (\x -> do
                       !a <- pE =<< addFlow name (n, x)
                       !b <- pE =<< addMon name (n, (x, Nothing))
                       !c <- pE =<< addTx name (n, (x, Nothing, Nothing))
                       return (a && b && c)
                     )
      pE :: Either SpiderException Bool -> SpiderM Bool
      pE r = case r of
        (Left e) -> (liftIO . print $ e) >> return False
        (Right a) -> return a
          -- saveGM _ n (Right r) = addMeshN (n, r)
          -- saveGM k n (Left s) = saveGrid k n s
          -- flR :: (Monad m) => FL.Fold m a c -> FL.Fold m (Either a b) (c, b)  
          -- flR rf = FL.partition rf idFold
