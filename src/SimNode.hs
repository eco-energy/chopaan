{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
module SimNode where
{--
import Lens.Micro
import Data.ProtoLens

import GHC.Generics
import Control.Monad.Bayes.Class

import Chopaan.Kibbutz.Transactor (runTransactor)
import Chopaan.Kibbutz.Kibbutz (kbtz, Kbtz, kbtzState)
import Chopaan.Comm.Comm (Address(..), Dispatch(..), MessageQs(..))
import Chopaan.Comm.Queues (initNodeQueue)
import Chopaan.Comm.Mqtt (Topic)
import Chopaan.Utils.Time
import Chopaan.Ui (mon)
import Chopaan.Node.Node (nodeS, SensorR)

import Proto.NodeMessageSchema.NodeMessages
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as F

import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Concurrent


import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.Compat (NominalDiffTime)
import Data.Time.LocalTime.Compat (LocalTime, addLocalTime, diffLocalTime)
import Data.Aeson
import Data.Hashable

import Control.Concurrent.STM (atomically)
--import Physics.Storage


import Streamly
import qualified Streamly.Prelude as S

import Env.MonadEnv (MonadEnv, sampleIOE)


import Servant.API.WebSocket (WebSocket)
import Network.Wai              (Application)
import Network.Wai.Handler.Warp (run)
import Network.WebSockets       (Connection, withPingThread, sendTextData)
import Servant                  ((:>), Proxy (..), Server, serve)
import Data.Aeson (ToJSON, FromJSON, encode)

uiDelay :: (MonadIO m) => m ()
uiDelay = liftIO . threadDelay $ 1000000




runtimeS :: forall t. (IsStream t) => t MonadEnv RuntimeStats
runtimeS = runtime
  where
    runtime :: t MonadEnv RuntimeStats
    runtime = forever $ do 
      S.yieldM runtimeDist

logsS :: forall t. (IsStream t) => t MonadEnv MeshFrame
logsS = logs
  where
    logs :: t MonadEnv MeshFrame
    logs = forever $ do
      S.yieldM logsDist





startDay :: TimeOfDay -> LocalTime
startDay = LocalTime $ fromGregorian 1 1 2020
    
runtimeDist :: (MonadSample m, MonadIO m) => m RuntimeStats
runtimeDist = do
  uiDelay
  return defMessage 

logsDist :: (MonadSample m, MonadIO m) => m MeshFrame
logsDist = do
  uiDelay
  return defMessage

temporalGaussians :: (MonadSample m) => (LocalTime, LocalTime) -> [(Double, Double)] -> (LocalTime -> m Double)
temporalGaussians _ [] _ = return 0
temporalGaussians (start, end) ranges@(r:rs) t = do
  let
    sections = length ranges
    sectionLength = diffLocalTime end start / (fromIntegral $ sections)
    t' = addLocalTime sectionLength start
  case isBetween start t' t of
    True -> (uncurry normal) r
    False -> temporalGaussians (t', end) rs t
  where
    isBetween s e x = x >= s && x <= e  



{------------------------------ Operational Stuff --------------------------------}


newtype NodeTest = NodeTest Int deriving (Eq, Ord, Show, Generic)

instance Hashable NodeTest
instance ToJSON NodeTest
instance FromJSON NodeTest

instance Address NodeTest where
  stateTopic = asTopic "state"
  controlTopic = asTopic "control"
  logTopic = asTopic "logs"
  fromStateTopic = fromTopic "state"
  fromControlTopic = fromTopic "control"
  fromLogTopic = fromTopic "logs"


asTopic :: T.Text -> NodeTest -> Topic
asTopic suffix (NodeTest n) = testPrefix <> suffix <> "/"  <> (toText n)
  where
    toText :: (Show a) => a -> T.Text
    toText = T.pack . show

fromTopic :: T.Text -> Topic -> Maybe NodeTest
fromTopic suffix t = case (isValidTopic t) of
       False  -> Nothing
       _ -> Just . NodeTest . read . T.unpack $ n
  where
    n = T.replace (suffix <> "/") "" $ T.replace prefix "" t
    isValidTopic t' = prefix `T.isPrefixOf` t' && suffix `T.isSuffixOf` t'
    prefix = testPrefix

testPrefix :: T.Text
testPrefix = "/kibbutz/test/node/"


{---------------------------------------------------------------------------------}


testKbtz :: forall t a b. (IsStream t, Dispatch a)
  => [NodeTest] -> (NodeTest -> t MonadEnv a) -> (t MonadEnv a -> t MonadEnv b) ->  (Kbtz t MonadEnv NodeTest b)
testKbtz = undefined -- kbtz @t @MonadEnv

testNodes :: [NodeTest]
testNodes = NodeTest <$> [1..]


type KbtzApi = "stream" :> WebSocket

server :: (ToJSON a) => (SerialT IO a) -> Server WebSocket
server s = streamData
 where
  streamData :: (MonadIO m) => Connection -> m ()
  streamData c = do
    liftIO $ withPingThread c 10 (print "replace this with what it appears for") $ (liftIO . S.mapM_ (sendTextData c . encode) $ s)


startApp :: IO ()
startApp = do
  let
    ns = take 10 testNodes
    sensors :: Kbtz SerialT MonadEnv NodeTest SensorR
    sensors = testKbtz ns (\_ -> nodeStream) (nodeS)
    --mesh = testKbtz ns (\_ -> meshStream) (meshT)
    --market = testKbtz ns (\_ -> txStream) (txS)
  --(s :: _) <- sampleIOE $ kbtzState sensors
  undefined

app :: (ToJSON a) => (SerialT IO a) -> Application
app s = serve api (server s)

api :: Proxy WebSocket
api = Proxy

testClient :: IO ()
testClient = undefined
{--
  outbox <- atomically $ initNodeQueue @NodeTest @MeshFrame
  let
    nodes = take 4 testNodes
    sensors = testKbtz @SerialT nodes (const nodeStream) nodeS
    runtime = testKbtz @SerialT nodes (const runtimeS) id
  (txMonitor, txs) <- sampleIOE $ runTransactor outbox (60*5) sensors
  return $ ()
  --mainWidget $ mon sampleIOE sensors runtime undefined txs txMonitor
--}
--}
