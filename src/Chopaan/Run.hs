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
import Chopaan.Comm.Comm (MessageQs(..), initQs, mkCallback, runNodeQueue, subStream)

import Chopaan.Kibbutz.Kibbutz (sensorKbtz, rsKbtz, getNodes, asStream, monitor, sub, runKbtz)
import Chopaan.Kibbutz.Transactor (runTransactor)

import Chopaan.UI (mon)
import qualified System.Remote.Monitoring as EKG
import qualified System.Metrics as EKG

import Control.Concurrent.STM.TBQueue

run :: RIO App ()
run = do
  app <- ask
  let
    Options{..} = appOptions app
    KibbutzOpts{..} = kibbutzOpts
  qs@MessageQs{..} <- liftIO . atomically $ initQs
  nodes <- runReaderT getNodes name
  _ <- liftIO $ forkIO $ forever $
       runMqtt mqttOpts outbox nodes (mkCallback qs)
  sensorStore <- liftIO $ EKG.newStore
  let 
    sensors' = sensorKbtz @SerialT @IO nodes stateQ
    runtime = rsKbtz @SerialT @IO nodes statsQ
  sensors <- liftIO $ monitor sensorStore sensors'
  (txMonitor, txs) <- liftIO $ runTransactor outbox (60*5) sensors
  _ <- liftIO $ EKG.forkServerWith sensorStore "localhost" 8000
  liftIO $ runKbtz sensors
