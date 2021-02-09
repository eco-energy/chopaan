{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses, GADTs, FlexibleInstances #-}
module Chopaan.Run (run, mon) where

import GHC.Generics
import Chopaan.Types
import RIO hiding (view, async)
import qualified Data.Text as Text
import Control.Concurrent (forkIO)

import Streamly
import qualified Streamly.Prelude as S

import Chopaan.Comm.Mqtt (runMqtt)
import Chopaan.Comm.Comm (MessageQs(..)
                         , initQs
                         , mkCallback
                         , Dispatch(..)
                         , Address(..)
                         , PubQueue
                         )
import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.Kibbutz ( sensorKbtz
                               , rsKbtz
                               , getNodes
                               , runKbtz
                               , Kbtz(..)
                               )
import Chopaan.Kibbutz.AWS.Things (withMqttAuth)
import Chopaan.Kibbutz.AWS.Common (newLogger, LogLevel(..))
import Chopaan.Kibbutz.Transactor (runTransactor, Tx(..), TransactionStatus, asKbtz)

import Chopaan.Ui (mon, defGrid)

import Chopaan.DB
import Chopaan.Utils.Retry
import Chopaan.Testing (testNodes, testPub)



data ChopaanState = ChopaanState
  { mqttClient :: Bool
  , serverHandle :: Bool
  } deriving (Eq, Ord, Show, Generic)


run :: RIO App ()
run = do
  app <- ask
  let
    Options{..} = appOptions app
    KibbutzOpts{..} = kibbutzOpts
    nodes = testNodes
  --nodes <- (runReaderT getNodes (KbtzId name))
  dbpool <- liftIO . (recoverC 100) $ dbPool dbOpts
  liftIO . print $ "DB Connection Pool Initialized"
  liftIO . print =<< (liftIO . (recoverC 1) . getSchema $ dbOpts)
  qs@MessageQs{..} <- liftIO $ initQs
  lg <- liftIO $ newLogger Debug stdout
  inbox <- liftIO . forkIO $
       withMqttAuth lg (KbtzId name)
         (runMqtt mqttOpts{connId=name} outbox nodes (mkCallback qs))
  outbox <- liftIO . forkIO $ testPub nodes outbox
  sensors <- liftIO $ sensorKbtz @SerialT @IO nodes stateChan
  runtime <- liftIO $ rsKbtz @SerialT @IO nodes statsChan
  liftIO $ print ("Running Monitor...")
  liftIO $ mon id (defGrid nodes) sensors runtime





