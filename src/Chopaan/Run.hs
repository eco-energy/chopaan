{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Chopaan.Run (run) where

import Chopaan.Node.Node (gridS, writeCSVRecords, NodeS)
import Chopaan.Types
import RIO
import Control.Concurrent (forkIO)

import Streamly
import qualified Streamly.Prelude as S

import Control.Monad.State.Lazy (runStateT)

import RIO.Time

import Chopaan.Comm.Mqtt (runMqtt)
import Chopaan.Comm.Comm (MessageQs(..), initQs, mkCallback, subStream)
import Chopaan.Kibbutz.Kibbutz (Kbtz(..), sensorKbtz, logsKbtz, rsKbtz, getNodes, asFRPNetwork)
import Chopaan.UI.Monitor (monitor, MonitorC)
import Chopaan.Node.NodeId
import Proto.NodeMessageSchema.NodeMessages
import Reflex.Vty (mainWidget, VtyWidget, runVtyApp)
import Reflex
import Reflex.Spider
import Control.Monad.Trans

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
  liftIO $ mainWidget $ mon sensors runtime logs 
  where
    writer stateMap = runStateT (writeCSVRecords "test.csv" stateMap) (UTCTime (fromGregorian 1 1 2020) 0)


mon :: forall m m' t' t a. (TriggerEvent t' m', MonitorC t' m', MonadAsync m, IsStream t, Show a)
  => Kbtz t m NodeMAC NodeS
  -> Kbtz t m NodeMAC RuntimeStats
  -> Kbtz t m NodeMAC a
  -> VtyWidget t' m' (Event t' ()) --VtyWidget t' m (Event t' ())
mon sensors runtime logs = do
  s <- asFRPNetwork sensors
  r <- asFRPNetwork runtime
  l <- asFRPNetwork logs
  monitor s r l
