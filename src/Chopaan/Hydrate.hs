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


type S3S (t :: (* -> *) -> * -> *) m a b = t m (Either a b)


s3Stream' :: (IsStream t, MonadAsync m, MonadCatch m)
         => S3Opts
         -> [(NodeMAC, Maybe ObjectKey)]
         -> (UTCTime, UTCTime)
         -> M.Map NodeMAC (t m (Either EnergyState RuntimeStats))
s3Stream' bucket ns range = M.fromList $ fmap (\(n, o) -> (n, nodeS3 bucket range n o)) ns



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

type Hydration' = ((NodeMAC, ObjectKey, Maybe UTCTime)
                 , ((M.Map NodeMAC SensorR)
                   , (M.Map NodeMAC (MeshNode, RxSignal))))

type Hydration = Bool

type Range = (UTCTime, UTCTime)

hydrateKbtz :: forall t. (IsStream t) => KbtzC NodeMAC -> Range -> GraphM (t GraphM Bool)
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
                                               $ S.mapM (pure . (const True))
                                               $ S.minRate 10000
                                               $ S.concatFoldableWith S.parallel
                                               $ grider $ mesher lsyncs 
        where
          mesher :: [(NodeMAC, Maybe ObjectKey)]
               -> M.Map NodeMAC (t GraphM (Either EnergyState (MeshNode, RxSignal)))
          mesher lsyncs = M.mapWithKey (\n s ->
                                        S.trace (saveM n) $ S.mapM (pure . mfn) $ s) $
                        s3Stream' bucket lsyncs range
            where
              mfn :: (Either x RuntimeStats)
                -> (Either x (MeshNode, RxSignal)) 
              mfn = (fmap ((meshNodeLink (getGridRoot name))))
              saveM :: NodeMAC -> (Either x (MeshNode, RxSignal)) -> GraphM Bool
              saveM n x = case x of
                (Left _) -> return False
                (Right r) -> do
                  --liftIO $ print r >> return False
                  withSpider $ addMeshN (n, r)
          grider :: M.Map NodeMAC (t GraphM (Either EnergyState (MeshNode, RxSignal)))
            -> M.Map NodeMAC (t GraphM SensorR)
          grider = M.mapWithKey (\n s -> S.trace -- (liftIO . print)
                                                 (withSpider . (saveGrid name n))
                                         $ S.postscan sensorFold
                                         $ S.lefts s)
          prefix = "processing rate: "
          saveGrid k n = (\x -> do
                a <- addFlow k (n, x)
                b <- addMon k (n, (x, Nothing))
                c <- addTx k (n, (x, Nothing, Nothing))
                return ((a && b && c) `seq` x )
                         )
