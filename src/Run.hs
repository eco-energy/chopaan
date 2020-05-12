{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import UI (runTUI, mkUIChan, genTick)
import Mqtt (runMqtt)
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
import Control.Monad.State.Lazy (runStateT)

import RIO.Time

run :: RIO App ()
run = do
  app <- ask
  let
    Options{..} = appOptions app
    KibbutzOpts{..} = kibbutzOpts
  k@Kibbutz{..} <- liftIO $ getKibbutz name
  uiChan <- liftIO $ mkUIChan
  kmState <- liftIO $ atomically $ initKMS nodes
  kConnM <- liftIO $ atomically $ initKMConn nodes
  _ <- liftIO $ forkIO $ forever $ runMqtt mqttOpts outQueue nodes (mkCallback k)
  _ <- liftIO $ forkIO $ stream nodes inQueue uiChan kmState
  liftIO $ runTUI k kmState kConnM uiChan
  where
    updateKMIO monitor stateMap = liftIO . atomically $ updateKMAll monitor stateMap (uncurry nmFilter)
    stream nodes inQueue uiChan kmState = gridS nodes (subStream inQueue) &
                                          S.trace (writer) &
                                          S.mapM_ (\x -> (updateKMIO kmState x) >> (genTick uiChan))
    writer stateMap = runStateT (writeCSVRecords "test.csv" stateMap) (UTCTime (fromGregorian 1 1 2020) 0)
