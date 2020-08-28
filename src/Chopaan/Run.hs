{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Chopaan.Run (run) where

import Chopaan.Node.Node (gridS, writeCSVRecords)
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
import Chopaan.UI.Monitor (monitor)

import Reflex.Vty (mainWidget, VtyWidget)

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
  liftIO $ mainWidget $ do
      s <- asFRPNetwork sensors
      r <- asFRPNetwork runtime
      l <- asFRPNetwork logs
      monitor s r l
  where
    writer stateMap = runStateT (writeCSVRecords "test.csv" stateMap) (UTCTime (fromGregorian 1 1 2020) 0)
    --app :: IO ()
    --app = mainWidget $ monitor sensors runtime logs
