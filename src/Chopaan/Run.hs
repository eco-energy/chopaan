{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Chopaan.Run (run, mon, monitorRS) where

import Chopaan.Types
import RIO
import Control.Concurrent (forkIO)

import Streamly
import qualified Streamly.Prelude as S
import Chopaan.Comm.Mqtt (runMqtt)
import Chopaan.Comm.Comm (MessageQs(..), initQs, mkCallback)

import Chopaan.Kibbutz.Kibbutz (sensorKbtz, logsKbtz, rsKbtz, getNodes, asStream)
import Chopaan.Kibbutz.Transactor (monitorTx, planTx)

import Chopaan.UI (mon)

import Reflex.Vty (mainWidget)


run :: RIO App ()
run = do
  app <- ask
  let
    Options{..} = appOptions app
    KibbutzOpts{..} = kibbutzOpts
  qs@MessageQs{..} <- liftIO . atomically $ initQs
  nodes <- runReaderT getNodes name
  sensors <- liftIO $ sensorKbtz @SerialT nodes stateQ
  runtime <- liftIO $ rsKbtz @SerialT nodes statsQ
  logs    <- liftIO $ logsKbtz @SerialT nodes logsQ
  _ <- liftIO $ forkIO $ forever $
       runMqtt mqttOpts outbox nodes (mkCallback qs)
  liftIO $ mainWidget $ mon id sensors runtime logs



monitorRS :: RIO App ()
monitorRS = do
  app <- ask
  let
    Options{..} = appOptions app
    KibbutzOpts{..} = kibbutzOpts
  qs@MessageQs{..} <- liftIO . atomically $ initQs
  nodes <- runReaderT getNodes name
  runtime <- liftIO $ rsKbtz @SerialT nodes statsQ
  liftIO $ forever $
       runMqtt mqttOpts outbox nodes (mkCallback qs)
