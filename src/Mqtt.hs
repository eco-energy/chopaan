{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings#-}

module Mqtt where


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
import Control.Concurrent (threadDelay)
import Data.Maybe

import qualified Data.Map.Strict as Map

import Control.Concurrent.STM
import qualified Control.Concurrent.STM.TQueue as TQ

import Registry (NodeT, HasTopics(..), mkCallback, Kibbutz(..))


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

runMqtt :: Kibbutz -> IO ()
runMqtt k@Kibbutz {..}  = do
  tlsConf <- mkTLSSettings certPath keyPath mqttURI connId
  let
    (Just uri) = parseURI $ Text.unpack $ mqttURI <> "#" <> connId 
    conf = MQ.mqttConfig
           { MQ._protocol=MQ.Protocol311
           , MQ._connID="chopaan-pilot"
           , MQ._msgCB=mkCallback k 
           , MQ._connectTimeout=18000000000
           , MQ._tlsSettings=tlsConf}
  
  _ <- forkIO $ forever $ printer monitorStateT
  forever $ catches (go conf uri stopics dispatchQueueT) [Handler (\(ex :: MQ.MQTTException) -> handler (show ex))]
  where
    connId = "chopaan-pilot"
    mqttURI = "mqtts://a1e7lyi19kctcn-ats.iot.ap-southeast-1.amazonaws.com"
    thingTypeName = "kibbutz-pilot-node"
    certPath = "certs/chopaan.cert.pem"
    keyPath = "certs/chopaan.private.key.pem"
    go c u ts dq = do
      mc <- MQ.connectURI c u
      -- just passing a list of subscriptions to the subscribe function results in a call that gives a client error on aws.
      print =<< mapM (\t-> MQ.subscribe mc [t] []) ts
      _ <- forkIO $ forever $ pubQueue mc dq
      MQ.waitForClient mc
    
    handler e = putStrLn ("ERROR :" <> e) >> threadDelay 1000000
    printer :: TVar a -> IO ()
    printer st = do
       ns' <- atomically $ do 
         ns <- readTVar st
         return ns
       printAudit ns'
       threadDelay 10000000

    -- The pub queue is a concurrent friendly data structure. We also probably want to put the client in one. But clients are
    -- not stateful in haskell, are they?
    pubQueue :: MQ.MQTTClient -> TQ.TQueue a -> IO ()
    pubQueue c tv = do
      forever $ pub =<< (atomically $ do readTQueue tv)
      where
        pub (nId, mf) = MQ.publish c (topic nId) (pMsg mf) False
        topic n = "/kibbutz/node/" <> (unNodeId n) <> "/control"
        pMsg = BL.fromStrict . encodeMessage


-- https://stackoverflow.com/questions/40081508/how-to-provide-a-client-certificate-to-http-client-tls
mkTLSSettings :: FilePath -> FilePath -> Text.Text -> Text.Text -> IO TLSSettings
mkTLSSettings cert key hostName name = do
  creds <- either (error "couldn't read cert") Just <$> credentialLoadX509 cert key
  let
    hooks = def { onCertificateRequest = \_ -> return creds
                }
    clientParams = (defaultParamsClient (Text.unpack hostName :: HostName) ((BSC.pack . Text.unpack) name))
                  { clientHooks=hooks
                  , clientSupported = def {supportedCiphers=ciphersuite_strong}
                  }
  return (TLSSettings clientParams)

