{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import UI (runTUI, mkUIChan, genTick)
import Mqtt (runMqtt, defMQOpts)
import Registry (
  Kibbutz(..), getKibbutz, mkCallback
  , initKMS, initKMConn, subStream
  )

import Node (gridS, nmFilter, writeCSVRecords)
import Import
import Control.Concurrent (forkIO)

import Streamly ()
import qualified Streamly.Prelude as S

import StateMonitor (updateKMAll)

run :: RIO App ()
run = do
  k@Kibbutz{..} <- liftIO $ getKibbutz thingTypeName
  uiChan <- liftIO $ mkUIChan
  kmState <- liftIO $ atomically $ initKMS nodes
  kConnM <- liftIO $ atomically $ initKMConn nodes
  _ <- liftIO $ forkIO $ forever $ runMqtt defMQOpts outQueue nodes (mkCallback k)
  _ <- liftIO $ forkIO $ stream nodes inQueue uiChan kmState
  liftIO $ runTUI k kmState kConnM uiChan
  where
    thingTypeName = "kibbutz-pilot-node"
    updateKMIO monitor stateMap = liftIO . atomically $ updateKMAll monitor stateMap (uncurry nmFilter)
    stream nodes inQueue uiChan kmState = gridS nodes (subStream inQueue) &
                                          S.trace (\_ -> genTick uiChan) &
                                          S.trace (writeCSVRecords "test.csv") &
                                          S.mapM_ (updateKMIO kmState)
{--

import Proto.NodeMessages ()
import Proto.NodeMessages_Fields

import Data.ProtoLens (defMessage)
import Lens.Micro

import qualified Prelude as P (reverse, head, print)

tqueue <- liftIO (atomically $ initNodeQueue) :: RIO App (NodeQueue NodeT EnergyState)
_ <- liftIO $ forkIO $ forever $ demoTx tqueue

demoTx :: NodeQueue NodeT EnergyState -> IO ()
demoTx oQ = do
  let e = (take 10 es)
  _ <- mapM (uncurry $ writeToNodeQueue oQ) e 
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
--}
