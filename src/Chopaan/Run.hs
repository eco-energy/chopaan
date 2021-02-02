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
                         , writeToPubQ
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
import Chopaan.Kibbutz.Transactor (runTransactor, Tx(..), TransactionStatus, asKbtz)

import Chopaan.Ui (mon, defGrid)

import Chopaan.DB
import Chopaan.Utils.Retry


import Chopaan.Node.NodeId (NodeMAC, NodeId(..))


-- TESTING
import Proto.NodeMessageSchema.NodeMessages (EnergyState)
import Proto.NodeMessageSchema.NodeMessages_Fields
--


import Lens.Micro
import Data.ProtoLens


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
  qs@MessageQs{..} <- liftIO $ initQs nodes
  _ <- liftIO . forkIO $
       withMqttAuth
         (KbtzId name)
         (runMqtt mqttOpts{connId=name} outbox nodes (mkCallback qs))
  _ <- liftIO . forkIO $ testPub nodes outbox
  sensors <- liftIO $ sensorKbtz @SerialT @IO nodes stateChan
  runtime <- liftIO $ rsKbtz @SerialT @IO nodes statsChan
  liftIO $ print ("Running Monitor...")
  liftIO $ mon id (defGrid nodes) sensors runtime



k1Nodes :: [NodeMAC]
k1Nodes = NodeId <$> [ "24:6f:28:a9:71:30"
                     , "7c:9e:bd:f6:42:68"
                     , "7c:9e:b7:75:c6:cc"
                     , "7c:9e:bd:f5:07:c8"
                     , "7c:9e:bd:f6:44:98"
                     , "a4:cf:12:99:d3:d8"
                     , "24:6f:28:9d:43:48"
                     , "a4:cf:12:9a:39:4c"
                     ]


testNodes :: [NodeMAC]
testNodes = take 10 $
  (\ns -> NodeId (Text.intercalate (":" :: Text.Text) ns))
  <$> ((take 6) <$> (iterate twistor [a, b, c, d, e, f]))
  where
    a = "aa"
    b = "bb"
    c = "cc"
    d = "dd"
    e = "ee"
    f = "ff"
    g = "gg" :: Text.Text

twistor :: [a] -> [a]
twistor [] = []
twistor (x:xs) = (xs <> [x])

testPub :: [NodeMAC] -> PubQueue -> IO ()
testPub ns q = S.mapM_ (uncurry $ writeToPubQ q) $ constRate 1 $ asTopicDispatch <$> (simNodeES ns)
  where
    asTopicDispatch = stateTopic *** frame

simNodeES :: (MonadAsync m) => [NodeMAC] -> SerialT m (NodeMAC, EnergyState)
simNodeES (n:ns) = foldl' (wAsync) (es n) (es <$> ns) 
  where
    es :: (Monad m) => NodeMAC -> SerialT m (NodeMAC, EnergyState)
    es n = constRate 2 $ S.fromList [(n, m i) | i <- [1, 100..]]
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

