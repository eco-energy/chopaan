{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings#-}

module Chopaan.Comm.Mqtt (runMqtt, client, pub, MQ.Topic) where


-- Different string modules should be unified under one interface
import qualified Data.Text as Text 
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BSC



import qualified Network.MQTT.Client as MQ
import qualified Network.MQTT.Topic as MQ ()
import qualified Network.MQTT.Types as MQTy

import Network.Connection
import Network.TLS
import Data.X509.CertificateStore (readCertificateStore)
import Data.X509.Validation (validateDefault)

import Data.Default.Class
import Data.Maybe (fromJust)
import Network.TLS.Extra.Cipher
import Network.URI

import Control.Exception (Handler (..), catches)
import Control.Monad (forever, void)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM

import Data.ProtoLens (encodeMessage)

import Chopaan.Types (MQTTOpts(..))
import Chopaan.Comm.Comm (Address(..), Dispatch(..), PubQueue)
import Chopaan.Comm.Queues (NodeQueue(..))
import Chopaan.Utils.Retry
import Proto.NodeMessageSchema.NodeMessages (MeshFrame)



-- I want to setup an MQTT client that subscribes to kibuttz/node/{mac}/state and publishes to /kibbutz/node/{mac}/control


-- https://stackoverflow.com/questions/40081508/how-to-provide-a-client-certificate-to-http-client-tls
mkTLSSettings :: FilePath -> FilePath -> FilePath -> Text.Text -> Text.Text -> IO TLSSettings
mkTLSSettings cert key caPath hostName name = do
  creds <- either (error "Client Certificate Not Found") Just <$> credentialLoadX509 cert key
  --caCreds <- fromJust (error "CA Certificate Not Found") (readCertificateStore caPath)
  let
    hooks = def { onCertificateRequest = \_ -> return creds
                , onServerCertificate = \_ _ _ _ -> mempty -- validateDefault caCreds a b c
                }
    clientParams = (defaultParamsClient (Text.unpack hostName :: HostName) ((BSC.pack . Text.unpack) name))
                  { clientHooks=hooks
                  , clientSupported = def {supportedCiphers=ciphersuite_strong}
                  }
  return (TLSSettings clientParams)

-- need reader for creds and logs
runMqtt :: forall a. (Address a) => MQTTOpts -> PubQueue -> [a] -> MQ.MessageCallback -> IO ()
runMqtt opts outQueue ts msgCB = do
  mc <- client opts msgCB
  _ <- forkIO $ forever $ catches (pub mc outQueue) ((Handler . errorHandler) <$> ts)
  connStatus <- sequence $ (resub mc) <$> ts
  print connStatus
  MQ.waitForClient mc


client :: MQTTOpts -> MQ.MessageCallback -> IO (MQ.MQTTClient)
client MQTTOpts{..} msgCB = do
  tlsConf <- mkTLSSettings certPath keyPath caPath mqttURI connId
  let
    (Just uri) = parseURI $ Text.unpack $ mqttURI <> "#" <> connId
    conf = MQ.mqttConfig
           { MQ._protocol=MQ.Protocol311
           , MQ._connID=Text.unpack $ connId
           , MQ._port=443
           , MQ._msgCB=msgCB
           , MQ._connectTimeout=18000000
           , MQ._tlsSettings=tlsConf}
  MQ.connectURI conf uri

subscribe :: (Address n) => MQ.MQTTClient -> n -> IO (Either MQTy.SubErr MQ.QoS)
subscribe c n = head <$> (fst <$> MQ.subscribe c [subTopic n] [])

subTopic :: (Address n) => n -> (MQ.Topic, MQ.SubOptions)
subTopic n = (stateTopic n, MQ.subOptions { MQ._subQoS = MQ.QoS1 })

-- The pub queue is a concurrent friendly data structure. We also probably want to put the client in one. But clients are
    -- not stateful.
pub :: MQ.MQTTClient -> PubQueue -> IO ()
pub c tv = do
  forever $ pub' =<< (atomically $ do readTBQueue (runNodeQueue tv))
    where
      pub' :: (MQ.Topic, MeshFrame) -> IO ()
      pub' (nId, mf) = --putStrLn ("Publishing Message for topic: " <> (show $ nId)) >>
        MQ.publish c nId (encode mf) False
      encode = BL.fromStrict . encodeMessage

resub :: (Address n) => MQ.MQTTClient -> n -> IO (Either MQTy.SubErr MQ.QoS)
resub c n = (subscribe c n)--retryEither n (subscribe c)

errorHandler :: (Address n) => n -> MQ.MQTTException -> IO ()
errorHandler n (MQ.Timeout) = printError n "Timeout" 
errorHandler n (MQ.BadData) = printError n "BadData" 
errorHandler n (MQ.Discod d) = printError n d  
errorHandler n (MQ.MQTTException e) =  printError n e
printError n e = print $ (show e) <> "caught for Node:" <> show n
