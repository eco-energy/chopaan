{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import UI (runTUI, mkUIChan)
import Mqtt (runMqtt, defMQOpts)
import Transactor (runTransactor)
import StateMonitor (runStateMonitor)
import Registry (getKibbutz, nodes, mkCallback, outQueue, inQueue, queueStream, printQueueStream)

import Import
import qualified Streamly.Prelude as S
import Control.Concurrent (forkIO)


run :: RIO App ()
run = do
  kbtz <- liftIO $ getKibbutz thingTypeName
  uiChan <- liftIO mkUIChan
  _ <- liftIO $ forkIO $ runMqtt defMQOpts (outQueue kbtz) (nodes kbtz) (mkCallback kbtz uiChan)
  _ <- liftIO $ forkIO $ printQueueStream . queueStream . inQueue $ kbtz
  liftIO $ runTUI kbtz uiChan
  where
    thingTypeName = "kibbutz-pilot-node"
