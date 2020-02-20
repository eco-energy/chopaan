{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import UI (runTUI, mkUIChan, refreshTick)
import Mqtt (runMqtt, defMQOpts)
import Registry (
  Kibbutz(..), getKibbutz, mkCallback
  , KConnM, KMState, initKMS, initKMConn, NodeT
  , Outbox, initOutbox, writeToOutbox, SensorSM, SensorSub)

import Node (EnergyState, NodeS, NodeId(..), gridStream)
import Import
import Control.Concurrent (forkIO)

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL

import qualified Data.Time as Time
import StateMonitor (KibbutzMonitor, readKM, updateKM)


import Proto.NodeMessages ()
import Proto.NodeMessages_Fields

import Data.ProtoLens (defMessage)
import Lens.Micro
import qualified Prelude as P (reverse, head, print)
import Subscriber (subStream, subMap)


run :: RIO App ()
run = do
  k@Kibbutz{..} <- liftIO $ getKibbutz thingTypeName
  uiChan <- liftIO $ mkUIChan
  initTime <- liftIO $ Time.getCurrentTime
  kmState <- liftIO $ atomically $ initKMS nodes
  kConnM <- liftIO $ atomically $ initKMConn nodes
  tqueue <- liftIO (atomically $ initOutbox) :: RIO App (Outbox NodeT EnergyState)
  qs <- liftIO $ subMap nodes inQueue
  _ <- liftIO $ forkIO $ forever $ refreshTick 100 uiChan
  _ <- liftIO $ forkIO $ forever $ runMqtt defMQOpts tqueue nodes (mkCallback k)
  _ <- liftIO $ forkIO $ forever $ demoTx tqueue
  _ <- liftIO $ forkIO $ do
    --printS <- subStream inQueue (\(_, _) -> True)
    forever $ do
      S.mapM_ P.print (gridStream initTime nodes qs)
      P.print =<< (atomically $ readKM kmState nodes)
      P.print =<< (atomically $ readTVar msgCount)
      threadDelay 10000000
  liftIO $ S.drain (S.chunksOf 1 (updateFold' kmState) (gridStream initTime nodes qs))
  --liftIO $ runTUI k kmState kConnM uiChan
  where
    updateFold' :: MonadIO m => KMState -> FL.Fold m (NodeT, NodeS) ()
    updateFold' st = FL.drainBy (\(n, s)-> atomically $ updateKM st n s)
    thingTypeName = "kibbutz-pilot-node"
    demoTx :: Outbox NodeT EnergyState -> IO ()
    demoTx oQ = do
      let e = (take 10 es)
      _ <- mapM (uncurry $ writeToOutbox oQ) e 
      threadDelay 10000000


es :: [(NodeT, EnergyState)]
es = [((NodeId "24:0a:c4:c6:62:ac" :: NodeT), m i) | i <- [1, 100..]]
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
