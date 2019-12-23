{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

-- Protobuf Imports
import Proto.NodeMessages as NM
import Proto.NodeMessages_Fields as NM
import Data.ProtoLens (defMessage, showMessage, encodeMessage, decodeMessage)
import Lens.Micro
import Data.Word
import qualified Data.Text as Text
import qualified Data.ByteString as BS


-- AWS Imports
import qualified Network.AWS.IoT.ListThings as Iot
import qualified Network.AWS.IoT.Types as Iot
import Control.Monad.Trans.AWS
import Data.Maybe
import System.IO


-- MQTT Imports
import qualified Network.MQTT.Client as MQ
import qualified Network.MQTT.Topic as MQ
import Network.MQTT.Types (ConnACKFlags (..))
import Network.Connection
import Network.TLS
import Data.X509.CertificateStore
import Data.Default.Class 
import Network.TLS.Extra.Cipher
import Network.URI
import Control.Exception (Handler (..), IOException, catches)
import Control.Monad (forever, when)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM
import Control.Concurrent


eTR :: NM.EnergyTransactionRequest
eTR =
  defMessage
      & uuid .~ ("12" :: Text.Text)
      & dispatchedAt .~ (223123123 :: Word64)
      & powerInWatts .~ (100 :: Double)
      & durationInSeconds .~ (60*60 :: Word64)
      & direction .~ Outgoing


meshFrame :: NM.MeshFrame
meshFrame =
  defMessage
      & time .~ (3424234453 :: Word64)
      & transaction .~ eTR

{--
attrName :: Maybe Text.Text
attrName = Just "kibbutz"



kbtz :: Maybe Text.Text
kbtz = Just "PILOT"
--}

-- I want to setup an MQTT client that subscribes to kibuttz/node/{mac}/state and publishes to /kibbutz/node/{mac}/control
-- We obtain the list of nodes for a particular kibbutz by using the kibbutz name and querying AWS Iot for all thing names
-- for things which belong to that kibbutz.

----------------------------------------------------------------------------------
-- Metric Tracking

type WattSeconds = Double

type S = Double

data Metrics = Metrics
  { transmittedIn :: WattSeconds
  , transmittedOut :: WattSeconds
  , consumed :: WattSeconds
  , generated :: WattSeconds
  , tDiff :: S
  } deriving (Eq, Show, Ord)

instance Semigroup Metrics where
  m0 <> m1 = Metrics tIn tOut c g newTimeDiff
    where
      [tIn, tOut, c, g, newTimeDiff] = map (\t -> (sum $ map t ms)) ts 
      ms = [m0, m1]
      ts = [transmittedIn, transmittedOut, consumed, generated, tDiff]

instance Monoid Metrics where
  mempty = Metrics 0 0 0 0 0

processES :: S -> EnergyState -> Metrics
processES prevTime es = Metrics tIn tOut c g tdiff
  where
    tIn :: WattSeconds
    tIn = es ^. batteryVoltage * es ^. gridToBatteryCurrent 
    tOut :: WattSeconds
    tOut = es ^. batteryVoltage * es ^. batteryToGridCurrent
    c :: WattSeconds
    c = es ^. batteryVoltage * es ^. batteryToLoadCurrent
    g :: WattSeconds
    g = es ^. batteryVoltage * es ^. solarInputCurrent * timeInSeconds
    timeInSeconds = (fromIntegral $ es ^. cpuTime) / (1000 * 60) -- milliSToSeconds
    tdiff = timeInSeconds - prevTime

type NodeStates = Map.Map NodeID Metrics

updateNodeState :: NodeStates -> NodeID -> EnergyState -> NodeStates
updateNodeState ns n es = Map.adjust updateMetric n ns
  where
    updateMetric = (<> processES s es)
    s = tDiff $ ns Map.! n

----------------------------------------------------------------------------------

type StateTopic = Text.Text
type ControlTopic = Text.Text

type ThingName = Text.Text

newtype NodeID = NodeID { unNodeID :: ThingName } deriving (Eq, Show, Ord)



getThings :: Text.Text -> IO [Iot.ThingAttribute]
getThings thingTypeName = do
  let
    iiot = Iot.ioT{_svcPrefix="execute-api"} :: Service 
    ttn = (Just thingTypeName) :: Maybe Text.Text
  lgr <- newLogger Trace stdout
  env <- newEnv Discover <&> set envLogger lgr . set envRegion Singapore <&> configure iiot
  runResourceT . runAWST env $ do
    things <- send (Iot.listThings & Iot.ltThingTypeName .~ ttn)
    --   putStrLn (show (map (\t -> (fromMaybe ("NotFound", "NotFound") t)) topics))
    return $ things ^. Iot.ltrsThings


thingName :: Iot.ThingAttribute -> Maybe Text.Text
thingName t = t ^. Iot.taThingName


nameToTopics :: ThingName -> (StateTopic, ControlTopic)
nameToTopics name = (st, ct)
  where
    st :: StateTopic
    st = prefix <> n <> stChannel
    ct :: ControlTopic
    ct = prefix <> n <> cChannel
    stChannel = "/state"
    cChannel = "/control"
    prefix = "/kibbutz/node/"
    n = Text.replace ":" "" name

stateTopicToNodeID :: Text.Text -> NodeID
stateTopicToNodeID t = NodeID n
  where
    n = Text.replace "/state" "" $ Text.replace "/kibbutz/node" "" t
    

-- https://stackoverflow.com/questions/40081508/how-to-provide-a-client-certificate-to-http-client-tls
mkTLSSettings :: Text.Text -> Text.Text -> IO TLSSettings
mkTLSSettings hostName name = do
  creds <- either (error "couldn't read cert") Just <$> credentialLoadX509 cert key
  let
    hooks = def { onCertificateRequest = \_ -> return creds
                , onServerCertificate = \_ _ _ _ -> return []
                }
    clientParams = (defaultParamsClient hostName name)
                  { clientHooks=hooks
                  , clientSupported = def {supportedCiphers=ciphersuite_strong}
                  }
  return (TLSSettings clientParams)
  where
    cert = "certs/chopaan.cert.pem"
    key = "certs/chopaan.private.key.pem"

initMonitorState :: [ThingName] -> NodeStates
initMonitorState ts = Map.fromList [((NodeID t), mempty) | t <- ts]


main :: IO ()
main = do
  things <- getThings thingTypeName
  tlsConf <- mkTLSSettings mqttURI connID
  monitorState <- atomically $ newTVar initMonitorState
  let
    topics = (fmap nameToTopics) <$> map thingName things
    stopics :: [(Text.Text, MQ.SubOptions)]
    stopics = zip ts subopts
      where
        ts = (map ((fromMaybe "NoTopic") . fmap fst) topics)
        subopts = (repeat MQ.subOptions)
    cb _ t m p =  print (t, msg parsed, l)
      where
        nodeId = stateTopicToNodeID t
        msg (Left a) = show a
        msg (Right s) = updateNodeState s
        parsed :: Either String EnergyState
        parsed = decodeMessage $ toStrict m
        l = BL.length m
        toStrict = BS.concat . BL.toChunks
        
    (Just uri) = parseURI $ mqttURI <> "#" <> connID 
    conf = MQ.mqttConfig
           { MQ._protocol=MQ.Protocol311
           , MQ._connID=connID
           , MQ._msgCB=MQ.SimpleCallback cb
           , MQ._connectTimeout=18000000000
           , MQ._tlsSettings=tlsConf}

  forever $ catches (go conf uri stopics) [Handler (\(ex :: MQ.MQTTException) -> handler (show ex))]
  where
    connID = "chopaan-pilot" :: Text.Text
    mqttURI = "mqtts://a1e7lyi19kctcn-ats.iot.ap-southeast-1.amazonaws.com" :: Text.Text
    thingTypeName = "kibbutz-pilot-node" :: Text.Text
    go c u ts = do
      mc <- MQ.connectURI c u
      print =<< MQ.subscribe mc ts []
      pub mc
      MQ.waitForClient mc
    

    handler e = putStrLn ("ERROR :" <> e) >> threadDelay 1000000
    pub :: MQ.MQTTClient -> IO ()
    pub c = do
      forever $ MQ.publish c "/kibbutz/node/3c71bf644520/control" pMsg False >> threadDelay 10000000
      where
        pMsg = BL.fromStrict $ encodeMessage meshFrame

data ChopaanOpts = ChopaanOpts
  { mqttURI :: Text.Text
  , connID :: Text.Text
  , thingTypeName :: Text.Text
  }

{--
import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_chopaan

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_chopaan.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run
--}
