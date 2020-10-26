{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Chopaan.Run (run, mon) where

import Chopaan.Types
import RIO
import Control.Concurrent (forkIO)

import Streamly
import qualified Streamly.Prelude as S
import Chopaan.Comm.Mqtt (runMqtt)
import Chopaan.Comm.Comm (MessageQs(..), initQs, mkCallback)

import Chopaan.Kibbutz.Kibbutz (sensorKbtz, logsKbtz, rsKbtz, getNodes, asMapStream)
import Chopaan.Kibbutz.Transactor (runTransactor)

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
  (txMonitor, txs) <- liftIO $ runTransactor outbox (60*5) sensors
  _ <- liftIO $ forkIO $ forever $
       runMqtt mqttOpts outbox nodes (mkCallback qs)
  liftIO $ mainWidget $ mon id sensors runtime logs txs txMonitor
  where
    printer :: (Show a) => Serial a -> IO ()
    printer s = (forkIO . (S.mapM_ print) $ s) >> return ()
  {--
  
--}
{--liftIO $ printer $ asMapStream sensors
  liftIO $ printer $ asMapStream runtime
  liftIO $ printer $ asMapStream logs
  liftIO $ printer txMonitor
  liftIO $ S.mapM_ print txs--}
