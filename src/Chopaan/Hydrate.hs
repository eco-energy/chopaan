{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes, ConstraintKinds, KindSignatures, QuantifiedConstraints, MultiParamTypeClasses, GADTs, FlexibleInstances#-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns  #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveAnyClass, DeriveFunctor, StandaloneDeriving, TupleSections, AllowAmbiguousTypes #-}

module Chopaan.Hydrate ( hydrateKbtz
                       , hydrateKbtz'
                       , hydrateKbtzM
                       , Hydration
                       ) where

import Control.Arrow
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch

import Streamly.Prelude as S (IsStream, MonadAsync, adapt)
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Fold.Tee as FL
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Pipe as P
import qualified Streamly.Data.Array.Foreign as A


import Data.Time
import Data.Maybe
import qualified Data.Map.Strict as M

import Network.AWS.S3

import Proto.NodeMessageSchema.NodeMessages (RuntimeStats, EnergyState)
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as N (cpuTime)

import Chopaan.Utils.Time
import Chopaan.Utils.Streamly
import Chopaan.Comm.S3
import Chopaan.Node.NodeId
import Chopaan.Node.Folds
import Chopaan.Node.Mesh
import Chopaan.Node.Metrics
import Chopaan.Graph
import Chopaan.Kibbutz


type S3S (t :: (* -> *) -> * -> *) m a b = t m ((NodeMAC, ObjectKey, Maybe UTCTime), Either (NodeMAC, a) (NodeMAC, b))


s3Stream :: (IsStream t, MonadAsync m, MonadCatch m)
         => S3Opts
         -> [(NodeMAC, Maybe ObjectKey)]
         -> (UTCTime, UTCTime)
         -> S3S t m EnergyState RuntimeStats
s3Stream bucket ns range = S.tapRate 10 (liftIO . (print . (prefix <>) . show))
                           $ S.maxRate 10000
                           S.|$ S.mapM (pure . (second f) . align)
                           S.|$ S.concatMapWith S.parallel (uncurry (nodeS3 bucket range))
                           S.|$ S.fromList ns
  where
    prefix = "combined rate: "
    -- (S.mergeBy onTime)
    onTime (_, ((_, a), _)) (_, ((_, b), _)) = fromMaybe EQ $ liftA2 compare a b
    align (a, ((b, c), d)) = ((b, a, c), (b, d)) 
    f (n, c) = case c of
      Left x -> Left (n, x)
      Right y -> Right (n, y)



hydrateKbtz' :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
  => DBPools -> KbtzC NodeMAC -> Range -> t m Hydration
hydrateKbtz' poo k = S.concatM . (hydrateKbtzM poo k)

hydrateKbtzM :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
  => DBPools -> KbtzC NodeMAC -> Range -> m (t m Hydration)
hydrateKbtzM poo k = (pure . S.adapt . S.hoist (runGraphWithDB poo))
                   <=< (runGraphWithDB poo . hydrateKbtz k)

type Hydration = ((NodeMAC, ObjectKey, Maybe UTCTime)
                 , ((M.Map NodeMAC SensorR)
                   , (M.Map NodeMAC (MeshNode, RxSignal))))

type Range = (UTCTime, UTCTime)

hydrateKbtz :: forall t. (IsStream t) => KbtzC NodeMAC -> Range -> GraphM (t GraphM (Hydration))
hydrateKbtz KbtzC{name, nodes, s3Opts} range = case s3Opts of
      Nothing -> return $ S.nil
      Just bucket -> do
        -- x <- liftIO $ newTVarIO (M.fromList [])
        -- lsyncs <- mapM (\n -> do
        --                    ls <- withKbtzPool (flip getNodeLastSync n) 
        --                    return $ (\a -> (n, ObjectKey <$> a)) (listToMaybe ls)
        --                ) nodes
        let
          lsyncs = zip nodes (repeat Nothing)
          
        -- let finalize = do
        --       m <- liftIO . atomically $ readTVar x
        --       mapM_ (\(n, ((ObjectKey k), _)) ->
        --                                withKbtzPool (\p -> addLastSyncToHH p n k)) $ M.toList m
        return $ S.tapRate 10 (liftIO . (print . (prefix <>) . show))
                                               $ (S.postscan process) S.|$ (srcs lsyncs)
        where
          srcs :: [(NodeMAC, Maybe ObjectKey)] -> t GraphM ((NodeMAC, ObjectKey, Maybe UTCTime), Either (NodeMAC, EnergyState) (NodeMAC, (MeshNode, RxSignal)))
          srcs lsyncs = S.trace (saveM) $ S.mapM (pure . mfn . fixMeshTS) $ s3Stream bucket lsyncs range
          {-# INLINE srcs #-}
          mfn :: (a, Either (n, x) (n, RuntimeStats))
            -> (a, Either (n, x) (n, (MeshNode, RxSignal))) 
          mfn = second (fmap (second (meshNodeLink (getGridRoot name))))
          {-# INLINE mfn #-}
          saveM :: (a, Either (NodeMAC, x) (NodeMAC, (MeshNode, RxSignal))) -> GraphM Bool
          saveM x = case (snd  x) of
            (Left _) -> return False
            (Right r) -> do
              withSpider $ addMeshN r
          {-# INLINE saveM #-}
          process :: FL.Fold GraphM ((NodeMAC, ObjectKey, Maybe UTCTime), Either (NodeMAC, EnergyState) (NodeMAC, (MeshNode, RxSignal))) Hydration
          process = secondF (eitherWalay)
          prefix = "processing rate: "
          fixMeshTS :: ((NodeMAC, ObjectKey, Maybe UTCTime)
                       , Either (NodeMAC, EnergyState) (NodeMAC, RuntimeStats))
            -> ((NodeMAC, ObjectKey, Maybe UTCTime)
               , Either (NodeMAC, EnergyState) (NodeMAC, RuntimeStats))
          fixMeshTS a@(_, (Left _)) = a
          fixMeshTS a@((_, _, Nothing), _) = a
          fixMeshTS a@((n, o, Just t), Right (n', r)) = case r ^? N.cpuTime of
            Nothing -> ((n, o, Just t), Right (n', r & N.cpuTime .~ (timeToUIntSeconds t)))
            (Just t') -> case (t' == 0) of
              True -> ((n, o, Just t), Right (n', r & N.cpuTime .~ (timeToUIntSeconds t)))
              False -> a
          saveGrid k n = (\x -> do
                a <- addFlow k (n, x)
                b <- addMon k (n, (x, Nothing))
                c <- addTx k (n, (x, Nothing, Nothing))
                return ((a && b && c) `seq` x )
                         )
          eitherWalay :: FL.Fold GraphM (Either (NodeMAC, EnergyState) (NodeMAC, (MeshNode, RxSignal))) ((M.Map NodeMAC SensorR), (M.Map NodeMAC (MeshNode, RxSignal)))
          eitherWalay = FL.partition
            ((FL.demux $ M.fromList $ fmap (\n -> (n, FL.rmapM (withSpider . saveGrid name n) sensorFold)) nodes))
            ((FL.demux $ M.fromList $ fmap (\n -> (n, idFold)) nodes))
