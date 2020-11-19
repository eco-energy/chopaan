{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Chopaan.Run (run, mon) where

import Chopaan.Types
import RIO hiding (view)
import Control.Concurrent (forkIO)

import Streamly
import qualified Streamly.Prelude as S

import Chopaan.Comm.Mqtt (runMqtt)
import Chopaan.Comm.Comm (MessageQs(..), initQs, mkCallback, writeToNodeQ, Dispatch(..), subStream)

import Chopaan.Kibbutz.Kibbutz (sensorKbtz, rsKbtz, getNodes, monitor, asStream, runKbtz, logKbtz)
import Chopaan.Kibbutz.Transactor (runTransactor)

import Chopaan.UI (mon)
import qualified System.Remote.Monitoring as EKG
import qualified System.Metrics as EKG


{-- TESTING --}
import qualified Data.Map.Strict as M
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
    nodes = [(NodeId "24:0a:c4:c6:62:ac" :: NodeMAC), NodeId "24:0a:c4:c6:62:ad", NodeId "24:0a:c4:c6:62:ae",  NodeId "24:0a:c4:c6:62:af"]
    --nodes = [(NodeId "24:0a:c4:c6:62:ac" :: NodeMAC)]
  --nodes <- runReaderT getNodes name
  qs@MessageQs{..} <- liftIO . atomically $ initQs nodes
  _ <- liftIO $ forkIO $ forever $
       runMqtt mqttOpts outbox nodes (mkCallback qs)
  sensorStore <- liftIO $ EKG.newStore
  _ <- liftIO $ forkIO $ S.mapM_ (\(i, e) -> do
                            threadDelay 1000000
                            print "Dispatching!"
                            writeToNodeQ (stateQs M.! i) i e >>= print) $ foldl' (<>) S.nil (es <$> nodes)
  let
    sensors' = sensorKbtz @SerialT @IO nodes stateQs
    runtime = rsKbtz @SerialT @IO nodes statsQs
  liftIO $ runKbtz @SerialT $ logKbtz sensors'
  --runKbtz @SerialT $ traceKbtz (const print) $ sensors'
  sensors <- liftIO $ monitor sensorStore sensors'
  (txMonitor, txs) <- liftIO $ runTransactor outbox (60*5) sensors
  liftIO $ EKG.registerGcMetrics sensorStore
  _ <- liftIO $ EKG.forkServerWith sensorStore "localhost" 8000
  liftIO $ runKbtz sensors




es :: (Monad m) => NodeMAC -> SerialT m (NodeMAC, EnergyState)
es n = S.fromList [(n, m i) | i <- [1, 100..]]
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
