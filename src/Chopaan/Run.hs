{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses, GADTs, FlexibleInstances #-}
module Chopaan.Run (run, mon) where

import GHC.Generics
import Chopaan.Types
import RIO hiding (view, async, withAsync, Async)
import qualified Data.Text as Text
import Control.Concurrent (forkIO)
import qualified Control.Concurrent.Async as A

import Streamly
import qualified Streamly.Prelude as S

import Chopaan.Node.NodeId (NodeMAC)
import Chopaan.Node.Folds (SensorS)
import Proto.NodeMessageSchema.NodeMessages (RuntimeStats, HardwareConfig)
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
import Chopaan.Kibbutz.Transactor (runTransactor, Tx(..), TransactionStatus)
import Chopaan.Kibbutz.Mesh


import Chopaan.Server (mon, defGrid)
import Kbtz
import Chopaan.DB
import Chopaan.Utils.Retry
import Chopaan.Testing (testNodes, testPub)


data ChopaanState = ChopaanState
  { mqttClient :: Bool
  , serverHandle :: Bool
  } deriving (Eq, Ord, Show, Generic)


data KbtzSpec k n c = KbtzSpec { kbtzId :: k
                               , kbtznodes :: [(n, HardwareConfig)]
                             } deriving (Eq, Ord, Show, Generic)

run :: RIO App ()
run = do
  app <- ask
  let
    Options{..} = appOptions app
    KibbutzOpts{..} = kibbutzOpts
    --nodes = testNodes
  --nodes <- (runReaderT getNodes (KbtzId name))
  --  kibbutzim = []
  --dbpool <- liftIO . (recoverC 100) $ dbPool dbOpts
  --liftIO . print $ "DB Connection Pool Initialized"
  liftIO . print =<< (liftIO . (recoverC 500) . getSchema $ dbOpts)
  --liftIO $ mapM (runKibbutz @AheadT @IO mqttOpts{connId=name}) kibbutzim
  --liftIO $ print ("Running Monitor...")
  --liftIO $ mon id (defGrid nodes) sensors runtime

type Kibbutzim t m n a = (IsStream t, MonadAsync m) => Map KbtzName (Kbtz t m n a)





runKibbutz :: forall t m. (KbtzM m NodeMAC) => MQTTOpts -> KbtzName -> m ()
runKibbutz mqttOpts name = do
  ns <- nodes name
  lg <- liftIO $ newLogger Debug stdout
  qs' <-  (liftIO $ A.async (liftIO $ mqtt lg ns))
  MessageQs{stateChan, statsChan, outbox} <- liftIO $ A.wait qs'
  sensors <- sensorKbtz @AsyncT ns stateChan
  runtime <- runKbtz <$> rsKbtz @AsyncT ns statsChan
  let txStatus = runTransactor outbox (10 * 60) sensors
  return ()
  where
    mqtt lg nodes = liftIO $
       withMqttAuth lg name
         (runMqtt mqttOpts nodes mkCallback)
