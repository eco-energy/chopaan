{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes, ConstraintKinds, KindSignatures, QuantifiedConstraints, MultiParamTypeClasses, GADTs, FlexibleInstances#-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns  #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveAnyClass, DeriveFunctor, StandaloneDeriving, TupleSections, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chopaan.Kibbutz where

import GHC.Generics

import Network.AWS.S3 (BucketName, ObjectKey(..))
import Chopaan.Types hiding (DBOpts)

import Control.Applicative
import Control.Arrow
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Catch
import Control.Concurrent (forkIO)
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import qualified Data.Map as M
import Data.Maybe
import qualified Control.Concurrent.Async as A

import Streamly as S
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold.Types as FL
import qualified Streamly.Internal.Data.Pipe as P

import Proto.NodeMessageSchema.NodeMessages (RuntimeStats, EnergyState)
import System.IO (stdout)

import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.Kibbutz

import Chopaan.Comm.Mqtt.AWS (withMqttAuth)
import Chopaan.Kibbutz.AWS.Common (newLogger, LogLevel(..))
import Chopaan.Kibbutz.Transactor
import Chopaan.Node.NodeId (NodeMAC)
import Chopaan.Node.Folds (SensorR, sensorFold, meshFold)
import Chopaan.Node.Metrics (initSM)


import Chopaan.Comm.Mqtt (runMqtt)
import Chopaan.Comm.S3
import Chopaan.Comm.Comm (MessageQs(..)
                         , mkCallback
                         , PubQueue
                         , unfoldChan
                         , initQs
                         , writeChan
                         , Address
                         )
import Chopaan.Graph.Kbtz
import Chopaan.Graph
import Data.Time

type S3Opts = BucketName
type ChannelOpts = (MessageQs NodeMAC)

data KbtzC n = KbtzC
  { name :: KbtzName
  , nodes :: [n]
  , channelOpts :: ChannelOpts
  , s3Opts :: Maybe S3Opts
  } deriving (Generic)


mkKbtzConf :: KbtzName -> [n] -> ChannelOpts -> Maybe S3Opts -> KbtzC n
mkKbtzConf = KbtzC

qSrc :: forall t m n. (KbtzConn t m n)
  => MessageQs n
  -> m (t m (n, EnergyState), t m (n, RuntimeStats), PubQueue)
qSrc (MessageQs{stateChan, statsChan, outbox}) = do
  sk <- unfoldChan stateChan
  rk <- unfoldChan statsChan
  return $ (sk, rk, outbox)


twoSrc :: (KbtzConn t m n)
  => MessageQs n
  -> MessageQs n
  -> m (t m (n, EnergyState), t m (n, RuntimeStats), PubQueue)
twoSrc q q' = do
  (e, r, p) <- qSrc q
  (e', r', p') <- qSrc q'
  return $ (e `parallel` e', r `parallel` r', p')


type S3S (t :: (* -> *) -> * -> *) m a b = t m ((NodeMAC, ObjectKey, Maybe UTCTime), Either (NodeMAC, a) (NodeMAC, b))


s3Stream :: (IsStream t, MonadAsync m, MonadCatch m)
         => [(NodeMAC, Maybe ObjectKey)]
         -> S3Opts
         -> S3S t m EnergyState RuntimeStats
s3Stream ns bucket = let
  s' = S.bracketIO (liftIO $ newLogger Info stdout) (pure) s
  x :: (a, ((b, c), d)) -> ((b, a, c), (b, d))
  x (a, ((b, c), d)) = ((b, a, c), (b, d)) 
  f :: (n, Either a b) -> Either (n, a) (n, b)
  f (n, c) = case c of
    Left x -> Left (n, x)
    Right y -> Right (n, y)
  in (fmap (second f) $ fmap x s')
  where
    s lg = S.concatMapWith S.parallel (uncurry (nodeS3 lg bucket)) $ S.fromList ns
      
-- s3Src :: forall m. (MonadAsync m, MonadCatch m)
--   => [(NodeMAC, Maybe ObjectKey)]
--   -> S3Opts
--   -> m (MessageQs NodeMAC)
-- s3Src ns bucket = do
--    qs <- liftIO $ initQs
--    liftIO . forkIO . S.drain . S.aheadly $ S.mapM (\(ok, ((n, t), x)) -> case x of
--               Left e -> liftIO $ writeChan (stateChan qs) n e
--               Right r -> liftIO $ writeChan (statsChan qs) n r
--           ) (s3Stream ns bucket)
--    return qs


mqttQs :: (MonadIO m) => MQTTOpts -> KbtzName -> [NodeMAC] -> m (MessageQs NodeMAC)
mqttQs opts name ns = do
  lg <- liftIO $ newLogger Info stdout
  liftIO $ (A.wait
              =<< A.async (liftIO $ withMqttAuth lg name
                            (runMqtt ns mkCallback opts)))

mqttSrc :: forall t m. (KbtzConn t m NodeMAC) => KbtzName -> [NodeMAC] -> MQTTOpts
  -> m ((t m (NodeMAC, EnergyState), t m (NodeMAC, RuntimeStats), PubQueue))
mqttSrc k ns o = qSrc  =<< (mqttQs o k ns)



runKibbutz' :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
  => String -> Int -> KbtzC NodeMAC -> t m Bool
runKibbutz' h p = S.concatM . (runKibbutzM h p)

runKibbutzM :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
  => String -> Int -> KbtzC NodeMAC -> m (t m Bool)
runKibbutzM h p = (pure . S.adapt . S.hoist (runGraphM h p)) <=< (runGraphM h p . runKibbutz)



runKibbutz :: forall t. (IsStream t) => KbtzC NodeMAC -> GraphM (t GraphM Bool)
runKibbutz KbtzC{name, nodes, channelOpts, s3Opts} = do
  -- Live Data
  (es, rs, outbox) <- qSrc @t channelOpts
  -- Folds
  gridFold <- withSpider $ saveTx name
  meshF <- withSpider addMeshNode
  -- Stream Processors that run Folds
  let
      processES = tapCount "statePipe"
                  . S.postscan gridFold
                  . S.map getLatest
                  . status
                  -- . S.trace (dispatchTxSafe outbox . snd . snd)
                  . plan
                  . (fmap (second Tx))
                  . gridSensorR nodes

      processRS = tapCount "rsPipe" . S.postscan meshF
  let
    liveStream = (processRS rs) `parallel` (processES es)
  return liveStream
  where
    plan = S.postscan (secondF (dupF (transactionPlanner horizon)))
    {-# INLINE plan #-}
    status = S.postscan (secondF (txFold (Tx . M.fromList $ [(n, mempty @Stake) | n <- nodes])))
    {-# INLINE status#-}
    getLatest ::
      (NodeMAC, ((NodeStates NodeMAC, Maybe (TxPlan NodeMAC)), (TxState NodeMAC)))
      -> (NodeMAC, (SensorR, Maybe Stake, Maybe TxStatus))
    getLatest (n, ((Tx a, b), c)) = let
      a' = fromMaybe initSM (M.lookup n a)
      b' = (\x -> M.lookup n (unTx x)) =<< b
      c' = snd <$> (M.lookup n (unTx c))
      in (n, (a', b', c'))
    {-# INLINE getLatest #-}
    tapCount = S.tap . printCount
    printCount s = FL.mkFoldId (\x a ->
                                  (liftIO . print $ s <> ": " <> (show x) <> "is: " <> (show a))
                                  >> (return $ x + (1 :: Int)))
                   (pure 0)
    --tryToBool = expToBool <=< try
    dispatchTxSafe o t = tryJust t
      where
        tryJust (Just x) = expToBool
                         =<< (try $ (dispatchTx o x))
        tryJust Nothing = pure False
    horizon = 10 * 60

gridSensorR :: (KbtzConn t m n) => [n] -> t m (n, EnergyState) -> t m (n, M.Map n SensorR)
gridSensorR ns s = S.map (first fromJust)
                S.|$ S.filter (isJust . fst)
                S.|$ S.postscan ((,)
                               <$> (FL.mkPureId ((const (Just . fst))) Nothing)
                               <*> sensorFD) s
  where
    sensorFD = FL.demux $ M.fromList $ (, sensorFold) <$> ns
    {-# INLINE sensorFD #-}
{-# INLINE gridSensorR #-}

hydrateKbtz' :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
  => String -> Int -> KbtzC NodeMAC -> t m Bool
hydrateKbtz' h p = S.concatM . (hydrateKbtzM h p)

hydrateKbtzM :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
  => String -> Int -> KbtzC NodeMAC -> m (t m Bool)
hydrateKbtzM h p = (pure . S.adapt . S.hoist (runGraphM h p)) <=< (runGraphM h p . hydrateKbtz)

hydrateKbtz :: forall t. (IsStream t) => KbtzC NodeMAC -> GraphM (t GraphM Bool)
hydrateKbtz KbtzC{name, nodes, s3Opts} = case s3Opts of
      Nothing -> return $ S.nil
      Just s -> do
        x <- liftIO $ newTVarIO (M.fromList [])
        lsyncs <- mapM (\n -> do
                           ls <- withKbtzPool (flip getNodeLastSync n) 
                           return $ (\x -> (n, ObjectKey <$> x)) (listToMaybe ls)
                       ) nodes
        flowFoldS3 <- withSpider $ addFlowNode name
        meshFoldS3 <- withSpider addMeshNode
        let
          srcs :: t GraphM ((NodeMAC, ObjectKey, Maybe UTCTime), Either (NodeMAC, EnergyState) (NodeMAC, RuntimeStats))
          srcs = s3Stream lsyncs s
          gridEventFold = fmap getLatest (gridSensorF @t nodes)
          eitherWalay :: FL.Fold GraphM (Either (NodeMAC, EnergyState) (NodeMAC, RuntimeStats)) ((NodeMAC, SensorR), Bool)
          eitherWalay = FL.partition gridEventFold meshFoldS3
          thatF = FL.drainBy (\(n, k, t) ->
                                liftIO . atomically $ modifyTVar' x (M.insert n (k, t)))
          tpl = FL.unzip thatF eitherWalay
        --withKbtzPool $ (flip addLastSyncToHH)
        let finalize = do
              m <- liftIO . atomically $ readTVar x
              mapM_ (\(n, ((ObjectKey k), _)) ->
                                       withKbtzPool (\s -> addLastSyncToHH s n k)) $ M.toList m
        return
          $ S.finallyIO finalize
          $ S.postscan flowFoldS3
          S.|$ S.map (fst . snd)
          S.|$ S.postscan tpl srcs
      where
        getLatest :: (NodeMAC, M.Map NodeMAC SensorR) -> (NodeMAC, SensorR)
        getLatest (n, a) = (n, fromMaybe initSM (M.lookup n a))


gridSensorF :: forall t m n. (KbtzConn t m n) => [n]
            -> FL.Fold m (n, EnergyState) (n, M.Map n SensorR)
gridSensorF ns = let
  f :: FL.Fold m (n, EnergyState) (Maybe n, M.Map n SensorR)
  f = (,) <$> (FL.mkPureId ((const (Just . fst))) Nothing) <*> sensorFD
  f' = fmap (first fromJust) f
  in f'
  where
    sensorFD = FL.demux $ M.fromList $ (, sensorFold) <$> ns
    {-# INLINE sensorFD #-}
{-# INLINE gridSensorF #-}
