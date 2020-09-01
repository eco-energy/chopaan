{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Chopaan.Run (run, mon) where

import Chopaan.Node.Node (NodeS)
import Chopaan.Types
import RIO
import Control.Concurrent (forkIO)

import Streamly
import Chopaan.Comm.Mqtt (runMqtt)
import Chopaan.Comm.Comm (Address, MessageQs(..), initQs, mkCallback)

import Chopaan.Kibbutz.Kibbutz (Kbtz(..), sensorKbtz, logsKbtz, rsKbtz, getNodes, asFRPNetwork)

import Chopaan.UI.Base (UIConstraints)
import Chopaan.UI.Monitor (monitor)
import Proto.NodeMessageSchema.NodeMessages
import Reflex.Vty (mainWidget, VtyWidget)
import Reflex


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


mon :: forall t' t m m' n a. (IsStream t, MonadAsync m, MonadIO m', TriggerEvent t' m', UIConstraints t' m', Address n, Ord n, Show n, Show a)
  => (forall x. m x -> IO x)
  -> Kbtz t m n NodeS
  -> Kbtz t m n RuntimeStats
  -> Kbtz t m n a
  -> VtyWidget t' m' (Event t' ()) --VtyWidget t' m (Event t' ())
mon h sensors runtime logs = do
  s <- asFRPNetwork h sensors
  r <- asFRPNetwork h runtime
  l <- asFRPNetwork h logs
  monitor s r l
