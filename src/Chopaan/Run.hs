{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses, GADTs, FlexibleInstances #-}
module Chopaan.Run (run, mon) where

import Chopaan.Types
import RIO hiding (view, async)
import qualified Data.Text as Text
import Control.Concurrent (forkIO)

import Streamly
import qualified Streamly.Prelude as S

import Chopaan.Comm.Mqtt (runMqtt)
import Chopaan.Comm.Comm (MessageQs(..)
                         , initQs
                         , mkCallback
                         , Dispatch(..)
                         , Address(..)
                         , writeToPubQ
                         , PubQueue
                         )
import Chopaan.Kibbutz.Kibbutz (sensorKbtz, rsKbtz, getNodes, monitor, taggedS, runKbtz, logKbtz)
import Chopaan.Kibbutz.Transactor (runTransactor)

import Chopaan.UI (mon)
import qualified System.Remote.Monitoring as EKG
import qualified System.Metrics as EKG
import Chopaan.DB
import Chopaan.DB.Sensors

{-- TESTING --}

import Chopaan.Node.NodeId (NodeMAC, NodeId(..))
import Proto.NodeMessageSchema.NodeMessages (EnergyState)
import Proto.NodeMessageSchema.NodeMessages_Fields



import Lens.Micro
import Data.ProtoLens



run :: RIO App ()
run = do
  app <- ask
  let
    Options{..} = appOptions app
    KibbutzOpts{..} = kibbutzOpts
  nodes <- (runReaderT getNodes name)
  liftIO $ print nodes
  dbConns <- liftIO $ getDbConn dbOpts
  qs@MessageQs{..} <- liftIO $ initQs nodes
  _ <- liftIO $ forkIO $ forever $
       runMqtt mqttOpts outbox nodes (mkCallback qs)
  --_ <- liftIO . forkIO $ testPub nodes outbox 
  sensors' <- liftIO $ sensorKbtz nodes stateChan
  runtime <- liftIO $ rsKbtz @SerialT @IO nodes statsChan
  liftIO $ S.drain . runKbtz . logKbtz $ sensors'
  --sensorStore <- liftIO $ EKG.newStore
  --sensors <- liftIO $ monitor sensorStore sensors'
  --(txMonitor, txs) <- liftIO $ runTransactor outbox (60*5) sensors
  --liftIO $ EKG.registerGcMetrics sensorStore
  --_ <- liftIO $ EKG.forkServerWith sensorStore "localhost" 8000
  --liftIO $ forkIO $ runKbtz $ logKbtz runtime
  --liftIO $ runKbtz sensors


testNodes :: [NodeMAC]
testNodes = take 5 $ NodeId <$> [Text.pack $ [a] <> [b] <> [c] <> [d]
                       | a <- "acdsdfsv", b <- "casdaf"
                       , c <- "asdsad", d <- "asdasda"]

testPub :: [NodeMAC] -> PubQueue -> IO ()
testPub ns q = S.mapM_ (uncurry $ writeToPubQ q) $ constRate 1 $ asTopicDispatch <$> (simNodeES ns)
  where
    asTopicDispatch = stateTopic *** frame

simNodeES :: (MonadAsync m) => [NodeMAC] -> SerialT m (NodeMAC, EnergyState)
simNodeES (n:ns) = foldl' (wAsync) (es n) (es <$> ns) 
  where
    es :: (Monad m) => NodeMAC -> SerialT m (NodeMAC, EnergyState)
    es n = constRate 2 $ S.fromList [(n, m i) | i <- [1, 100..]]
    m :: Int -> EnergyState
    m t = defMessage
          & batteryVoltage .~ 12
          & gridVoltage .~ 60
          & batteryToLoadCurrent .~ 5
          & batteryToGridCurrent .~ 5
          & gridToBatteryCurrent .~ 0
          & solarInputCurrent .~ 10
          & dutyCycle .~ 0
          & cpuTime .~ (fromIntegral $ (1581444138 + t))
