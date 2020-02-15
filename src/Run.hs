{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import UI (runTUI, mkUIChan, refreshTick)
import Mqtt (runMqtt, defMQOpts)
import Registry (getKibbutz, mkCallback, Kibbutz(..), nodeStream, updateMonitorState)
import Node (NodeS)
import Import
import Control.Concurrent (forkIO)
import Streamly
import qualified Streamly.Prelude as S
import qualified Data.Time as Time
import StateMonitor (initKMS, initKMConn, NodeT)

run :: RIO App ()
run = do
  k@Kibbutz{..} <- liftIO $ getKibbutz thingTypeName
  uiChan <- liftIO $ mkUIChan
  initTime <- liftIO $ Time.getCurrentTime
  kmState <- liftIO $ atomically $ initKMS nodes
  kConnM <- liftIO $ atomically $ initKMConn nodes
  _ <- liftIO $ forkIO $ refreshTick 1000 uiChan
  _ <- liftIO $ forkIO $ runMqtt defMQOpts outQueue nodes (mkCallback k)
  let
    ns :: (IsStream t) => t IO (NodeT, NodeS)
    ns = nodeStream initTime nodes inQueue
  _ <- liftIO $ forkIO $ S.drain $ S.mapM (\s' -> atomically $ updateMonitorState kmState kConnM s') ns
  liftIO $ runTUI k kmState kConnM uiChan
  where
    thingTypeName = "kibbutz-pilot-node"
