{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes, ConstraintKinds, KindSignatures, QuantifiedConstraints, MultiParamTypeClasses, GADTs, FlexibleInstances#-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns  #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveAnyClass, DeriveFunctor, StandaloneDeriving, TupleSections, AllowAmbiguousTypes, BangPatterns #-}

module Chopaan.Hydrate ( hydrateKbtz
                       , hydrateKbtz'
                       , hydrateKbtzM
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

import Network.AWS.S3 (s3, ObjectKey)
import Chopaan.Kibbutz.AWS.Common (getAwsEnv, Env)

import Proto.NodeMessageSchema.NodeMessages (RuntimeStats, EnergyState)

import Chopaan.Comm.S3
import Chopaan.Node.NodeId
import Chopaan.Node.Folds
import Chopaan.Node.Mesh
import Chopaan.Graph
import Chopaan.Kibbutz



s3Stream' :: (IsStream t, MonadAsync m, MonadCatch m)
  => Env
  -> S3Opts
  -> [(NodeMAC, Maybe ObjectKey)]
  -> (UTCTime, UTCTime)
  -> M.Map NodeMAC (t m (Either EnergyState RuntimeStats))
s3Stream' env b ns range = M.fromList $ fmap (\(n, o) -> (n, nodeS3 env b range n o)) ns


hydrateKbtz' :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
             => DBPools
             -> KbtzC NodeMAC
             -> Range
             -> t m Hydration
hydrateKbtz' poo k = S.concatM . (hydrateKbtzM poo k)

hydrateKbtzM :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
             => DBPools
             -> KbtzC NodeMAC
             -> Range
             -> m (t m Hydration)
hydrateKbtzM poo k = (pure . S.adapt . S.hoist (runGraphWithDB poo))
                   <=< (runGraphWithDB poo . hydrateKbtz k)

--type Hydration' = (NodeMAC, Either SensorR (MeshNode, RxSignal))

type Hydration = Bool

type Range = (UTCTime, UTCTime)

-- concatIxFoldable :: forall t m n a.
--   (Ord n, IsStream t, MonadAsync m) => M.Map n (t m a) -> t m (n, a)
-- concatIxFoldable = M.foldrWithKey c S.nil
--   where
--     c :: n -> t m a -> t m (n, a) -> t m (n, a)
--     c k v acc = (fmap (k,) v) `S.parallel` acc 


hydrateKbtz :: forall t. (IsStream t) => KbtzC NodeMAC -> Range -> GraphM (t GraphM Hydration)
hydrateKbtz KbtzC{name, nodes, s3Opts} range = case s3Opts of
      Nothing -> return $ S.nil
      Just bucket -> do
        env <- getAwsEnv s3
        let
          lsyncs = zip nodes (repeat Nothing)
        return $ S.tapRate 10 (liftIO . (print . (prefix <>) . show))
          $ S.concatFoldableWith S.parallel --IxFoldable
          $ grider
          $ mesher
          (s3Stream' env bucket lsyncs range) -- & mesher & grider 
        where
          tapCount :: forall m a. (MonadAsync m, Show a) => String -> t m a -> t m a
          tapCount n = S.tap (printCount n)
          printCount s = FL.foldlM' (\x a ->
                                  (liftIO . print $ s <> ": " <> (show x))
                                  >> (return $ x + (1 :: Int))) (pure 1)
          mesher :: M.Map NodeMAC (t GraphM (Either EnergyState RuntimeStats))
               -> M.Map NodeMAC (t GraphM (Either EnergyState (MeshNode, RxSignal)))
          mesher  = M.mapWithKey (\n s -> -- S.trace (saveM n) $
                                          S.map (mfn) $ s)               
            where
              mfn :: (Either x RuntimeStats)
                -> (Either x (MeshNode, RxSignal)) 
              mfn = (fmap ((meshNodeLink (getGridRoot name))))
              saveM :: NodeMAC -> (Either a (MeshNode, RxSignal)) -> GraphM Bool
              saveM n x = case x of
                (Left _) -> return True
                (Right r) -> do
                  withSpider $! addMeshN (n, r)
          grider :: M.Map NodeMAC (t GraphM (Either EnergyState (MeshNode, RxSignal)))
                 -> M.Map NodeMAC (t GraphM Bool) --(Either SensorR (MeshNode, RxSignal)))
          grider = M.mapWithKey (\n s -> S.map (const True)
                                         --  $ S.trace (withSpider . (saveGrid name n))
                                         $ S.postscan sensorFold
                                         $ S.lefts s)
          prefix = "Hydration Rate: "
          saveGrid k n = (\x -> do
                !a <- addFlow k (n, x)
                !b <- addMon k (n, (x, Nothing))
                !c <- addTx k (n, (x, Nothing, Nothing))
                return (a && b && c)
                         )
          -- saveGM _ n (Right r) = addMeshN (n, r)
          -- saveGM k n (Left s) = saveGrid k n s
          -- flR :: (Monad m) => FL.Fold m a c -> FL.Fold m (Either a b) (c, b)  
          -- flR rf = FL.partition rf idFold
