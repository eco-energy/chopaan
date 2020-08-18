{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Chopaan.Run (run) where

import Chopaan.Node.Node (Grid(..), gridS, writeCSVRecords)
import Chopaan.Types
import RIO
import Control.Concurrent (forkIO)

import Streamly
import qualified Streamly.Prelude as S

import Control.Monad.State.Lazy (runStateT)

import RIO.Time

import Chopaan.Comm.Mqtt (runMqtt)
import Chopaan.Comm.Comm (MessageQs(..), initQs, mkCallback, esStream, rtsStream)
import Chopaan.Kibbutz.Registry (Kibbutz(..), getKibbutz)


run :: RIO App ()
run = do
  app <- ask
  let
    Options{..} = appOptions app
    KibbutzOpts{..} = kibbutzOpts
  Kibbutz{nodes} <- liftIO $ getKibbutz name
  qs@MessageQs{..} <- liftIO . atomically $ initQs
  
  
  _ <- liftIO $ forkIO $ forever $ runMqtt mqttOpts outbox nodes (mkCallback qs)
  liftIO $ S.drain (stream nodes stateQ)
  where
    writer stateMap = runStateT (writeCSVRecords "test.csv" stateMap) (UTCTime (fromGregorian 1 1 2020) 0)
    stream nodes qs = gridS nodes (esStream qs) & S.trace (writer)
