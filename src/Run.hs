{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import UI (runTUI, mkUIChan, refreshTick, prepTx, Stake(..))
import Mqtt (runMqtt, defMQOpts)
import Registry (queueStream, runNodeQueue, writeToNodeQ, HasTopics, NodeQueue, getKibbutz, mkCallback, Kibbutz(..), nodeStream, updateMonitorState, writeToPubQ, initNodeQ)
import Node (EnergyState, NodeS, NodeId(..))
import Import
import Control.Concurrent (forkIO)

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL

import qualified Data.Time as Time
import StateMonitor (KConnM, KMState, initKMS, initKMConn, NodeT, getMonitorState)
import Data.Time.Clock (NominalDiffTime)


import Proto.NodeMessages ()
import Proto.NodeMessages_Fields
import Data.ProtoLens.Arbitrary

import Data.ProtoLens (Message, defMessage)
import Lens.Micro
import qualified Prelude as P (print)

run :: RIO App ()
run = do
  k@Kibbutz{..} <- liftIO $ getKibbutz thingTypeName
  uiChan <- liftIO $ mkUIChan
  initTime <- liftIO $ Time.getCurrentTime
  kmState <- liftIO $ atomically $ initKMS nodes
  kConnM <- liftIO $ atomically $ initKMConn nodes
  tqueue <- liftIO (atomically $ initNodeQ) :: RIO App (NodeQueue NodeT EnergyState)
  let
    ns :: SerialT IO (NodeT, NodeS)
    ns = nodeStream initTime nodes inQueue
  --_ <- liftIO $ forkIO $ S.drain $ S.trace P.print ns
  _ <- liftIO $ forkIO $ forever $ S.fold (updateFold kmState kConnM) (S.take 1 ns)
  --do
    --a <- S.head ns
    --(atomically $ ((updateMonitorState kmState kConnM)))
    --
  _ <- liftIO $ forkIO $ forever $ refreshTick 1000 uiChan
  _ <- liftIO $ forkIO $ forever $ runMqtt defMQOpts tqueue nodes (mkCallback k)
  _ <- liftIO $ forkIO $ forever $ demoTx tqueue
  liftIO $ forever $ do
     pp =<< (atomically $ getMonitorState kmState kConnM nodes)
     P.print =<< (atomically $ readTVar msgCount)
     threadDelay 10000000
  --liftIO $ runTUI k kmState kConnM uiChan
  where
    updateFold :: (MonadIO m) => KMState -> KConnM -> FL.Fold m (NodeT, NodeS) ()
    updateFold st' conn' = FL.drainBy (\s -> atomically $ updateMonitorState st' conn' s)
    pp ([], []) = return ()
    pp ((x:xs), (y:ys)) = P.print (x:xs, y:ys)
    thingTypeName = "kibbutz-pilot-node"
    demoTx :: NodeQueue NodeT EnergyState -> IO ()
    demoTx oQ = do
      mapM (uncurry $ writeToNodeQ oQ) (take 100 es)
      threadDelay 10000000
      where
        es = [((NodeId "240ac4c662ac" :: NodeT), m i) | i <- [1..]]
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
