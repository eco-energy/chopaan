{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

-- Protobuf Imports
import Proto.NodeMessages as NM
import Proto.NodeMessages_Fields as NM
import Data.ProtoLens (defMessage, showMessage, encodeMessage, decodeMessage)
import Lens.Micro
import Data.Word
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

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
import Control.Monad (forever, when, liftM)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM
import Control.Concurrent

-- Vis and CLI
import qualified Text.PrettyPrint.Tabulate as PPT
import GHC.Generics (Generic)
import Data.Data

-- Energy Transaction Stuff
import qualified Data.Time as Time
import Data.ULID (getULID)
import Data.Convertible
import Control.Concurrent.STM.TQueue

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

type Watts = Double

type S = Double

data Audit = Audit
  { transmittedIn :: WattSeconds
  , transmittedOut :: WattSeconds
  , consumed :: WattSeconds
  , generated :: WattSeconds
  , tDiff :: S
  } deriving (Eq, Show, Ord, Generic, Data)

instance Semigroup Audit where
  m0 <> m1 = Audit tIn tOut c g newTimeDiff
    where
      [tIn, tOut, c, g, newTimeDiff] = map (\t -> (sum $ map t ms)) ts
      ms = [m0, m1]
      ts = [transmittedIn, transmittedOut, consumed, generated, tDiff]

instance Monoid Audit where
  mempty = Audit 0 0 0 0 0

instance PPT.Tabulate Audit PPT.ExpandWhenNested

processES :: S -> EnergyState -> Audit
processES prevTime es = Audit tIn tOut c g tdiff
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

newtype NodeStates = NodeStates { unNodeStates :: Map.Map NodeId Audit } deriving (Show, Generic, Data)

instance PPT.CellValueFormatter NodeId


updateNodeState :: NodeStates -> NodeId -> EnergyState -> NodeStates
updateNodeState ns n es = NodeStates $ Map.adjust updateMetric n ns'
  where
    ns' = unNodeStates ns
    updateMetric = (<> processES s es)
    s = tDiff $ ns' Map.! n

printAudit :: NodeStates -> IO ()
printAudit ns = PPT.printTable $ unNodeStates ns

----------------------------------------------------------------------------------
-- Energy Transactor
-- 1) Generate Demand Events
-- 2) 
-- 3) 

type TransactionQ = TQueue (NodeId, NM.MeshFrame)

mkEnergyTransactionR :: Watts -> S -> NM.PDirection -> IO NM.EnergyTransactionRequest
mkEnergyTransactionR p t d = do
  ulid <- getULID
  time <- Time.getCurrentTime
  let 
   etr = defMessage
         & uuid .~ (Text.pack . show) ulid
         & dispatchedAt .~ (utcToWord64 time)
         & powerInWatts .~ p
         & durationInSeconds .~ (sToW64 t)
         & direction .~ d
   sToW64 :: S -> Word64
   sToW64 = convert
   utcToWord64 :: Time.UTCTime -> Word64
   utcToWord64 = c'' . c'
     where
       c' :: Time.UTCTime -> Int
       c' = convert
       c'' :: Int -> Word64
       c'' = convert
  return etr

class Frameable a where
  toMeshFrame :: a -> NM.MeshFrame
  fromMeshFrame :: NM.MeshFrame -> Maybe a


instance Frameable NM.EnergyTransactionRequest where
  toMeshFrame etr = undefined
  fromMeshFrame m = undefined

{--
data Transaction = Transaction
  { power :: Watts
  , edge :: (Int, Int)
  , time :: Integer
  , edgeCost :: Watts
  } deriving (Eq, Ord, Show, Generic)
--}


newtype VI a = VI { unVI :: (a, a)} deriving (Eq, Ord, Show, Generic, Functor)

data Transaction = Transaction
  { start :: Time.TimeOfDay,
    end   :: Time.TimeOfDay,
    nodes :: [VI Double]
  } deriving (Eq, Ord, Show)


transaction :: f g Audit -> f g Transaction
transaction = decode . attend . encode


encode :: f g a -> k b
encode = undefined
attend :: k b -> k c
attend = undefined
decode :: k c -> f g b
decode = undefined


----------------------------------------------------------------------------------
type StateTopic = Text.Text

type ControlTopic = Text.Text

type ThingName = Text.Text

newtype NodeId = NodeId { unNodeId :: ThingName } deriving (Eq, Show, Ord, Data, Generic)

getThings :: Text.Text -> IO [Iot.ThingAttribute]
getThings thingTypeName = do
  let
    iiot = Iot.ioT{_svcPrefix="execute-api"} :: Service 
    ttn = (Just thingTypeName) :: Maybe Text.Text
  lgr <- newLogger Trace stdout
  env <- newEnv Discover <&> set envLogger lgr . set envRegion Singapore <&> configure iiot
  runResourceT . runAWST env $ do
    things <- send (Iot.listThings & Iot.ltThingTypeName .~ ttn)
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

stateTopicToNodeId :: Text.Text -> NodeId
stateTopicToNodeId t = NodeId n
  where
    n = Text.replace "/state" "" $ Text.replace "/kibbutz/node" "" t
    

-- https://stackoverflow.com/questions/40081508/how-to-provide-a-client-certificate-to-http-client-tls
mkTLSSettings :: Text.Text -> Text.Text -> IO TLSSettings
mkTLSSettings hostName name = do
  creds <- either (error "couldn't read cert") Just <$> credentialLoadX509 cert key
  let
    hooks = def { onCertificateRequest = \_ -> return creds
                }
    clientParams = (defaultParamsClient (Text.unpack hostName :: HostName) ((BSC.pack . Text.unpack) name))
                  { clientHooks=hooks
                  , clientSupported = def {supportedCiphers=ciphersuite_strong}
                  }
  return (TLSSettings clientParams)
  where
    cert = "certs/chopaan.cert.pem"
    key = "certs/chopaan.private.key.pem"

initMonitorState :: [ThingName] -> NodeStates
initMonitorState ts = NodeStates $ Map.fromList [((NodeId t), mempty) | t <- ts]

--initTransaction 

runEnergyTransactor ts = undefined

main :: IO ()
main = do
  things <- getThings thingTypeName
  tlsConf <- mkTLSSettings mqttURI connId
  let
    tnames = map thingName things
    topics = (fmap nameToTopics) <$> tnames 
    stopics :: [(Text.Text, MQ.SubOptions)]
    stopics = zip ts subopts
      where
        ts = (map ((fromMaybe "NoTopic") . fmap fst) topics)
        subopts = (repeat MQ.subOptions{MQ._subQoS=MQ.QoS1})
  monitorStateT <- atomically $ newTVar $ initMonitorState (map (Text.replace ":" "") $ catMaybes tnames)
  dispatchQueueT <- atomically $ newTQueue @(NodeId, Transaction)
  let
    -- writes a dumb message to a dumb topic. 
    constantPublisher = do
      atomically $
        writeTQueue dispatchQueueT  ("test-123", Transaction  )
  let
    cb _ t m _ =  do
      print (t, parsed)
      _ <- atomically $ update
      return ()
      where
        update = do
          ns <- readTVar monitorStateT
          let
            update' (Right es) = updateNodeState ns nodeId es
          writeTVar monitorStateT (update' parsed)
          return ()
        nodeId = stateTopicToNodeId t
        parsed :: Either String EnergyState
        parsed = decodeMessage $ toStrict m
        toStrict = BS.concat . BL.toChunks
        
    (Just uri) = parseURI $ Text.unpack $ mqttURI <> "#" <> connId 
    conf = MQ.mqttConfig
           { MQ._protocol=MQ.Protocol311
           , MQ._connID="chopaan-pilot"
           , MQ._msgCB=MQ.SimpleCallback cb
           , MQ._connectTimeout=18000000000
           , MQ._tlsSettings=tlsConf}
  
  _ <- forkIO $ forever $ printer monitorStateT
  forever $ catches (go conf uri stopics dispatchQueueT) [Handler (\(ex :: MQ.MQTTException) -> handler (show ex))]
  where
    connId = "chopaan-pilot"
    mqttURI = "mqtts://a1e7lyi19kctcn-ats.iot.ap-southeast-1.amazonaws.com"
    thingTypeName = "kibbutz-pilot-node"
    go c u ts dq = do
      mc <- MQ.connectURI c u
      print =<< mapM (\t -> do putStrLn (show . fst $ t)) ts
      
      -- just passing a list of subscriptions to the subscribe function results in a call that gives a client error on aws.
      print =<< mapM (\t-> MQ.subscribe mc [t] []) ts
      _ <- forkIO $ forever $ pubQueue mc dq
      MQ.waitForClient mc
    
    handler e = putStrLn ("ERROR :" <> e) >> threadDelay 1000000
    printer :: TVar NodeStates -> IO ()
    printer st = do
       ns' <- atomically $ do 
         ns <- readTVar st
         return ns
       printAudit ns'
       threadDelay 10000000

    -- The pub queue is a concurrent friendly data structure. We also probably want to put the client in one. But clients are
    -- not stateful in haskell, are they?
    pubQueue :: MQ.MQTTClient -> TransactionQ -> IO ()
    pubQueue c tv = do
      forever $ pub =<< (atomically $ do readTQueue tv)
      where
        pub (nId, mf) = MQ.publish c (topic nId) (pMsg mf) False
        topic n = "/kibbutz/node/" <> (unNodeId n) <> "/control"
        pMsg = BL.fromStrict . encodeMessage




configureNode :: NodeId -> (NM.BatteryParameters, NM.PVParameters) -> NM.MeshFrame
configureNode = undefined



data ChopaanOpts = ChopaanOpts
  { mqttURI :: Text.Text
  , connId :: Text.Text
  , thingTypeName :: Text.Text
  }

data TransactionOpts = TransactionOpts
  { sources :: [Text.Text],
    sinks :: [Text.Text]
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
