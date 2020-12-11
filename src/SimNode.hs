{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
module SimNode where

import Lens.Micro
import Data.ProtoLens

import GHC.Generics
import Control.Monad.Bayes.Class

import Chopaan.Kibbutz.Transactor (runTransactor)
import Chopaan.Kibbutz.Kibbutz (kbtz, Kbtz, taggedS)
import Chopaan.Comm.Comm (Address(..), Dispatch(..), MessageQs(..))
import Chopaan.Comm.Queues (initNodeQueue)
import Chopaan.Comm.Mqtt (Topic)
import Chopaan.Utils.Time
import Chopaan.Utils.StreamsInterop (inIO)
import Chopaan.Run (mon)
import Chopaan.Node.Node (nodeS)

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
import Reflex.Vty (mainWidget)

import Servant.API.WebSocket (WebSocket)
import Network.Wai              (Application)
import Network.Wai.Handler.Warp (run)
import Network.WebSockets       (Connection, withPingThread, sendTextData)
import Servant                  ((:>), Proxy (..), Server, serve)
import Data.Aeson (ToJSON, FromJSON, encode)

uiDelay :: (MonadIO m) => m ()
uiDelay = liftIO . threadDelay $ 1000000



nodeStream :: forall t. (IsStream t) =>  t MonadEnv EnergyState
nodeStream = S.map snd $ S.iterateM (\xs -> do
                                        uiDelay
                                        nodeStep @MonadEnv xs) (pure (startDay $ TimeOfDay 0 0 0, defMessage))

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



nodeStep :: forall m. (MonadSample m) => (LocalTime, EnergyState) -> m (LocalTime, EnergyState)
nodeStep (t, oldState) = do
  -- note that outflow of current is assumed to be positive 
  loadCurrent <- abs <$> normal 30 20
  gridCurrent <- normal 0 20
  solarCurrent <- biGauss daytime (30, 10) (0, 0.3) t
  --solarVoltage <- biGauss daytime (17, 3) (0, 1) t
  batteryVoltageDiff <- normal 0.01 0.001
  gridVoltageDiff <- normal 0.03 0.03 
  
  let
    t' = addLocalTime (1 :: NominalDiffTime) t
    batteryV = oldState ^. F.batteryVoltage + batteryVoltageDiff
    gridV = oldState ^. F.gridVoltage + gridVoltageDiff
    
    newState = (defMessage :: EnergyState)
      & F.batteryVoltage .~ batteryV
      & F.gridVoltage .~ gridV
      & F.batteryToLoadCurrent .~ loadCurrent
      & F.batteryToGridCurrent .~ (if gridCurrent > 0 then gridCurrent else 0)
      & F.gridToBatteryCurrent .~ (if gridCurrent < 0 then gridCurrent else 0)
      & F.solarInputCurrent    .~ solarCurrent
      & F.temperature          .~ (26 :: Double)
      & F.cpuTime             .~  timeToUIntSeconds t
  return $ (t', newState)
  where
    biGauss :: (MonadSample m) => (t -> Bool) -> (Double, Double) -> (Double, Double) -> t -> m Double 
    biGauss choice (mu, theta) (mu', theta') chooser = case choice chooser of
      True -> normal mu theta
      False -> normal mu' theta'
    daytime :: LocalTime -> Bool
    daytime tx = t' > sunrise && t' < sunset
      where
        t' = localTimeOfDay tx
    (sunrise, sunset) = (TimeOfDay 6 0 0, TimeOfDay 18 0 0)

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
  let sensors = testKbtz @SerialT (take 10 testNodes) (\_ -> nodeStream) (nodeS)
  let s = inIO sampleIOE $ taggedS sensors
  putStrLn "Starting server on http://localhost:8080"
  run 8080 (app s)

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
