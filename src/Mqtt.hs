{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings#-}

module Mqtt (defMQOpts, runMqtt) where


-- Different string modules should be unified under one interface
import qualified Data.Text as Text 
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import qualified Data.Time as Time


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
import Control.Concurrent (forkIO, threadDelay)
import Data.Maybe

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TQueue as TQ

import Registry (NodeT, HasTopics(..), mkCallback, Kibbutz(..), PubQueue, runNodeQueue, KibbutzEvents)

import Data.ProtoLens (encodeMessage, Message)

import GHC.Generics (Generic)
import Brick.BChan (BChan)
-- I want to setup an MQTT client that subscribes to kibuttz/node/{mac}/state and publishes to /kibbutz/node/{mac}/control

{--


let
    tnames = map thingName things
    topics = (fmap nameToTopics) <$> tnames 
stopics :: [(Text.Text, MQ.SubOptions)]
stopics = zip ts subopts
where
ts = (map ((fromMaybe "NoTopic") . fmap fst) topics)
subopts = (repeat MQ.subOptions{MQ._subQoS=MQ.QoS1})
monitorStateT <- atomically $ newTVar $ initMonitorState (map (Text.replace ":" "") $ catMaybes tnames)
dispatchQueueT <- atomically $ newTQueue


--}

data MQTTOpts = MQTTOpts
  { connId :: Text.Text
  , mqttURI :: Text.Text
  , certPath :: FilePath
  , keyPath :: FilePath
  } deriving (Eq, Ord, Show, Generic)


defMQOpts :: MQTTOpts
defMQOpts = MQTTOpts {    connId = "chopaan-pilot"
                     ,    mqttURI = "mqtts://a1e7lyi19kctcn-ats.iot.ap-southeast-1.amazonaws.com"
                     ,    certPath = "certs/chopaan.cert.pem"
                     ,    keyPath = "certs/chopaan.private.key.pem"
                     }

-- need reader for creds and logs
runMqtt :: MQTTOpts -> Kibbutz -> BChan KibbutzEvents -> IO ()
runMqtt MQTTOpts{..} k@Kibbutz {..} brickChan = do
  tlsConf <- mkTLSSettings certPath keyPath mqttURI connId
  let
    (Just uri) = parseURI $ Text.unpack $ mqttURI <> "#" <> connId 
    conf = MQ.mqttConfig
           { MQ._protocol=MQ.Protocol311
           , MQ._connID=Text.unpack $ connId
           , MQ._msgCB=mkCallback k brickChan
           , MQ._connectTimeout=18000000000
           , MQ._tlsSettings=tlsConf}
    topics = zip (map stateTopic $ Set.toList nodes) $ repeat MQ.subOptions
  -- TODO: Add a logging Error Handler
  mc <- MQ.connectURI conf uri
  forkIO $ forever $ catches (sub mc topics) [Handler (\(ex :: MQ.MQTTException) -> handler (show ex))]
  forkIO $ forever $ catches (pub mc outQueue) [Handler (\(ex :: MQ.MQTTException) -> handler (show ex))]
  return ()
  where
    sub :: MQ.MQTTClient -> [(MQ.Filter, MQ.SubOptions)] -> IO ()
    sub c topics = do
      mapM (\t-> MQ.subscribe c [t] []) topics
      MQ.waitForClient c
    
    handler e = putStrLn ("ERROR :" <> e) >> threadDelay 1000000

    -- The pub queue is a concurrent friendly data structure. We also probably want to put the client in one. But clients are
    -- not stateful in haskell, are they?
    pub :: MQ.MQTTClient -> PubQueue -> IO ()
    pub c tv = do
      forever $ pub' =<< (atomically $ do readTQueue (runNodeQueue tv))
      where
        pub' :: (Message b) => (NodeT, b) -> IO ()
        pub' (nId, mf) = MQ.publish c (topic nId) (encode mf) False
        topic :: NodeT -> MQ.Topic
        topic = controlTopic
        encode :: (Message b) => b -> BL.ByteString
        encode = BL.fromStrict . encodeMessage


-- https://stackoverflow.com/questions/40081508/how-to-provide-a-client-certificate-to-http-client-tls
mkTLSSettings :: FilePath -> FilePath -> Text.Text -> Text.Text -> IO TLSSettings
mkTLSSettings cert key hostName name = do
  creds <- either (error "couldn't read cert") Just <$> credentialLoadX509 cert key
  let
    hooks = def { onCertificateRequest = \_ -> return creds
                , onServerCertificate = \_ _ _ _ -> return []
                }
    clientParams = (defaultParamsClient (Text.unpack hostName :: HostName) ((BSC.pack . Text.unpack) name))
                  { clientHooks=hooks
                  , clientSupported = def {supportedCiphers=ciphersuite_strong}
                  }
  return (TLSSettings clientParams)

