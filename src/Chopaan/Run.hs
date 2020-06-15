{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds, ConstrainedClassMethods#-}
module Chopaan.Run (run) where

import Chopaan.Node (Grid(..), gridS, writeCSVRecords, NodeId(..))
import Chopaan.Types
import RIO
import Control.Concurrent (forkIO)

import Streamly
import qualified Streamly.Prelude as S

import Control.Monad.State.Lazy (runStateT)

import RIO.Time

import Chopaan.Mqtt (runMqtt)
import Chopaan.Registry (
  Kibbutz(..), getKibbutz, mkCallback
  , subStream
  )


run :: RIO App ()
run = do
  app <- ask
  let
    Options{..} = appOptions app
    KibbutzOpts{..} = kibbutzOpts
  k@Kibbutz{..} <- liftIO $ getKibbutz name
  _ <- liftIO $ forkIO $ forever $ runMqtt mqttOpts outQueue nodes (mkCallback k)
  liftIO $ S.drain (stream nodes inQueue)
  where
    writer stateMap = runStateT (writeCSVRecords "test.csv" stateMap) (UTCTime (fromGregorian 1 1 2020) 0)
    stream nodes inQueue = gridS nodes (subStream inQueue) & S.trace (writer)
