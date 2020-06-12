{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds, ConstrainedClassMethods#-}
module Run (run) where

import Node (Grid(..), gridS, nmFilter, writeCSVRecords)
-- import Import
import Control.Concurrent (forkIO)

import Streamly ()
import qualified Streamly.Prelude as S

import StateMonitor (updateKMAll)
import Control.Monad.State.Lazy (runStateT)

import ConCat.Category
import RIO.Time

import UI (runTUI, mkUIChan, genTick)
import Mqtt (runMqtt)
import Registry (
  Kibbutz(..), getKibbutz, mkCallback
  , initKMS, initKMConn, subStream
  )


--data St a = (Node a, '[NodeId a])

stream :: NodeId -> [NodeId] -> _
stream nodes inQueue uiChan kmState = gridS nodes (subStream inQueue) &
                                      S.trace (writer) &
                                      S.mapM_ (\(Grid x) -> (updateKMIO kmState x) >> (genTick uiChan))


--run :: Streamly MqttConfig  
run = do
  app <- ask
  stream nodes inQueue rbehaviour 
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
    
    writer stateMap = runStateT (writeCSVRecords "test.csv" stateMap) (UTCTime (fromGregorian 1 1 2020) 0)
