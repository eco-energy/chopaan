{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import UI (runTUI, mkUIChan, refreshTick)
import Mqtt (runMqtt, defMQOpts)
import Registry (queueStream, runNodeQueue, writeToNodeQ, NodeQueue, getKibbutz, mkCallback, Kibbutz(..), nodeStream, updateMonitorState, initNodeQ)
import Node (EnergyState, NodeS, NodeId(..))
import Import
import Control.Concurrent (forkIO)

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL

import qualified Data.Time as Time
import StateMonitor (readKM, initKSensorM, updateKM, KibbutzMonitor, KConnM, KMState, initKMS, initKMConn, NodeT, getMonitorState)


import Proto.NodeMessages ()
import Proto.NodeMessages_Fields

import Data.ProtoLens (defMessage)
import Lens.Micro
import qualified Prelude as P (reverse, head, print)

run :: RIO App ()
run = do
  k@Kibbutz{..} <- liftIO $ getKibbutz thingTypeName
  uiChan <- liftIO $ mkUIChan
  initTime <- liftIO $ Time.getCurrentTime
  --kmState <- liftIO $ atomically $ initKMS nodes
  kConnM <- liftIO $ atomically $ initKMConn nodes
  tqueue <- liftIO (atomically $ initNodeQ) :: RIO App (NodeQueue NodeT EnergyState)
  kSensorM <- liftIO $ atomically $ initKSensorM nodes
  let
    ns :: SerialT IO (NodeT, NodeS)
    ns = nodeStream initTime nodes ((queueStream $ inQueue) :: SerialT IO (NodeT, EnergyState)) 
  _ <- liftIO $ forkIO $ forever $ refreshTick 100 uiChan
  _ <- liftIO $ forkIO $ forever $ runMqtt defMQOpts outQueue nodes (mkCallback k)
  --_ <- liftIO $ forkIO $ forever $ demoTx tqueue
  {--_ <- liftIO $ forkIO $ forever $ do
     P.print =<< (atomically $ readKM kSensorM nodes)
     P.print =<< (atomically $ readTVar msgCount)
     threadDelay 10000000--}
  _ <- liftIO $ forkIO $ S.mapM_ ((\(n, s)-> atomically $ updateKM kSensorM n s)) $ queueStream inQueue
  liftIO $ runTUI k kSensorM kConnM uiChan
  where
    --updateFold' :: (MonadIO m) => (KibbutzMonitor NodeT EnergyState) -> FL.Fold m (NodeT, EnergyState) ()
    --updateFold' st = FL.drainBy (\(n, s)-> atomically $ updateKM st n s)
    --updateFold :: (MonadIO m) => KMState -> KConnM -> FL.Fold m (NodeT, NodeS) ()
    --updateFold st' conn' = FL.drainBy (\s -> atomically $ updateMonitorState st' conn' s)
    --updateMonitor = S.sequence (\n-> atomically $ updateMonitorState kmState kConnM n) l
    --pp ([], []) = return ()
    --pp ((x:xs), (y:ys)) = P.print (x:xs, y:ys)
    thingTypeName = "kibbutz-pilot-node"
    --demoTx :: NodeQueue NodeT EnergyState -> IO ()
    --demoTx oQ = do
      --let e = (take 10 es)
      --(P.print $ (map (\e'-> (snd e')^. cpuTime)))
      --_ <- mapM (uncurry $ writeToNodeQ oQ) e 
      --threadDelay 10000000
      

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
