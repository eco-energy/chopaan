{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes, ConstraintKinds, KindSignatures, QuantifiedConstraints, MultiParamTypeClasses, GADTs, FlexibleInstances#-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns, NoMonomorphismRestriction  #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveAnyClass, DeriveFunctor, StandaloneDeriving, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chopaan.Kibbutz where

import GHC.Generics

import Data.Aeson (ToJSON, FromJSON)
import Data.Greskell
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)
import Data.Either
import Network.AWS.S3 (BucketName)
import Chopaan.Types hiding (DBOpts)
import Kbtz

import Control.DeepSeq (NFData)
import Control.Arrow (second, first)
import ConCat.Misc (result)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Concurrent (forkIO)
import qualified Data.Map as M
import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.STM as STM

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL

import Proto.NodeMessageSchema.NodeMessages (RuntimeStats, EnergyState)
import System.IO (stdout)


import Streamly.Prelude (IsStream, MonadAsync)


import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.Kibbutz

import Chopaan.Comm.Mqtt.AWS (withMqttAuth)
import Chopaan.Kibbutz.AWS.Common (newLogger, LogLevel(..))
import Chopaan.Kibbutz.Transactor (planTx
                                  , monitorTx
                                  , dispatchTx
                                  , TransactionStatus
                                  , TxPlan(..)
                                  , TxState(..)
                                  , Tx(..)
                                  , Stake
                                  , Role(..)
                                  , curryTx
                                  , stakeLinkDir
                                  , txStatusLinkDir
                                  )

import Chopaan.Node.NodeId (NodeId(..), NodeMAC)
import Chopaan.Node.Folds (SensorS, sensorFold, energyFold, demandFold, powerFold, timeFold, meshFold)
import Chopaan.Node.Node (nodeS)
import Chopaan.Node.Metrics (SensorMetrics(..), Node(..))
import Chopaan.Node.HW

import Chopaan.Node.Mesh

import Chopaan.Comm.Mqtt (runMqtt)
import Chopaan.Comm.S3
import Chopaan.Comm.Comm (MessageQs(..)
                         , mkCallback
                         , WriteChan
                         , PubQueue
                         , initPubQIO
                         , unfoldChan
                         , initQs
                         , writeChan
                         )
import Chopaan.Graph.Greskell
import Chopaan.Graph
import Chopaan.Graph.Spider
import NetSpider.Spider.Config


newtype Channel m n a = Channel {
  unChannel :: ChannelOpts -> (ParallelT m (n, a))
} deriving (Generic)


type S3Opts = BucketName
type ChannelOpts = Either (MessageQs NodeMAC) S3Opts



data KbtzC n = KbtzC
  { name :: KbtzName
  , nodes :: [n]
  , channelOpts :: ChannelOpts
  , spiderHost :: String
  , spiderPort :: Int
  } deriving (Generic)



mkKbtzConf :: KbtzName -> [n] -> ChannelOpts -> String -> Int -> KbtzC n
mkKbtzConf = KbtzC


qSrc :: forall t m. (KbtzConn t m NodeMAC)
  => MessageQs NodeMAC
  -> m (t m (NodeMAC, EnergyState), t m (NodeMAC, RuntimeStats), PubQueue)
qSrc (MessageQs{stateChan, statsChan, outbox}) = do
  sk <- unfoldChan stateChan
  rk <- unfoldChan statsChan
  return $ (sk, rk, outbox)


s3Qs :: forall m. (MonadAsync m)
  => [NodeMAC]
  -> S3Opts
  -> m (MessageQs NodeMAC)
s3Qs ns bucket = do
  lg <- liftIO $ newLogger Info stdout
  qs <- liftIO $ initQs
  let x = (first fst) <$> (S.concatMapWith parallel (nodeS3 lg bucket) $ S.fromList ns)
  liftIO . forkIO $ S.mapM_ (\(n, x) -> case x of
              Left e -> liftIO $ writeChan (stateChan qs) n e
              Right r -> liftIO $ writeChan (statsChan qs) n r
          ) x
  return $ qs 
  

mqttQs :: (MonadIO m) => MQTTOpts -> KbtzName -> [NodeMAC] -> m (MessageQs NodeMAC)
mqttQs opts name ns = do
  lg <- liftIO $ newLogger Info stdout
  liftIO $ (A.wait
              =<< A.async (liftIO $ withMqttAuth lg name
                            (runMqtt opts ns mkCallback)))

mqttSrc :: forall t m. (KbtzConn t m NodeMAC) => KbtzName -> [NodeMAC] -> MQTTOpts
  -> m ((t m (NodeMAC, EnergyState), t m (NodeMAC, RuntimeStats), PubQueue))
mqttSrc k ns o = qSrc  =<< (mqttQs o k ns)


runKibbutz :: forall t m. (IsStream t, MonadAsync m, MonadCatch m, Monad (t m)) => KbtzC NodeMAC -> m (t m Bool)
runKibbutz KbtzC{name, nodes, channelOpts, spiderHost, spiderPort} = do
  (es, rs, outbox) <- case channelOpts of
    Left queues -> qSrc @t queues
    Right s3Opts -> qSrc =<< s3Qs nodes s3Opts

  (esTimer, sensorS) <- duplicateS es
  
  let  
    sensorKbtz = S.postscan sensorFD sensorS
    powerK = _powerT <$> (Kbtz sensorKbtz)
    energyK = _energyT <$> (Kbtz sensorKbtz)
    storage = _battery <$> (Kbtz sensorKbtz)
    
  (txStream, dup1) <- duplicateS sensorKbtz
  (monStream, nodeStream) <- duplicateS dup1
  let
    txPlan = planTx horizon (Kbtz txStream)
    txMonitor = (flip monitorTx (Kbtz monStream)) <$> txPlan

    --dispatcher = S.mapM (dispatchTxSafe outbox) txPlan
    meshS = S.postscan rsFD rs
    kstate = gridState esTimer nodeStream txMonitor
    saveK = (uncurry (&&)) <$> S.scan saveTx kstate
    -- planHG = S.fold S.zipWith statePlanToFN $
    --         <$> (S.trace (liftIO . print) $ stream sensorKbtz)
    --         <*> ((curryTx mempty) <$> txPlan)
    -- monHG = ingestSensorKbtz $ editMonS snd $ (,)
    --         <$> stream sensorKbtz
    --         <*> ((fmap . fmap) (curryTx (Source, mempty)) txMonitor)
  return . adapt $ saveK `parallel` meshS {-- monHG `parallel` planHG `parallel` meshHG --}
  where
    sensorFD = FL.classify sensorFold
    rsFD = snd <$> ((,) <$> (FL.classify (meshFold)) <*> (addMeshNode))
    processEither = FL.partition sensorFD rsFD
    dispatchTxSafe o t = expToBool =<< (try $ dispatchTx o t)
    gridState :: () => t m (NodeMAC, EnergyState)
      -> t m (M.Map NodeMAC SensorS)
      -> t m (TxPlan NodeMAC, t m (TxState NodeMAC))
      -> t m (NodeMAC, (SensorS, Stake, TransactionStatus))
    gridState a st pl = do
      (n, _) <- a
      m <- st
      (Tx txMap, stati) <- pl
      (Tx statusMap) <- S.scan FL.mconcat stati 
      let
        sen = m M.! n
        tx = txMap M.! n
        mon = statusMap M.! n
      S.yield $ (n, (sen, tx, snd mon))
    horizon = 8



duplicateS
  :: MonadAsync m
  => IsStream t
  => t m a
  -> m (t m a, t m a)
duplicateS src = do
  (writeChan', readChan1, readChan2) <- liftIO $ do
    chan <- TChan.newBroadcastTChanIO
    chan' <- STM.atomically $ TChan.dupTChan chan
    chan'' <- STM.atomically $ TChan.dupTChan chan
    pure (chan, chan', chan'')
  let
    writes =
      S.mapM (liftIO . STM.atomically . TChan.writeTChan writeChan') src
    reads1 =
      S.repeatM (liftIO $ STM.atomically $ TChan.readTChan readChan1)
    reads2 =
      S.repeatM (liftIO $ STM.atomically $ TChan.readTChan readChan2)
  pure (fmap (fromRight undefined) $ S.filter isRight $ (Left <$> writes) `S.async` (Right <$> reads1), reads2)
