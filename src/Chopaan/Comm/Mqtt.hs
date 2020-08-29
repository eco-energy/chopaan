{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings#-}

module Chopaan.Comm.Mqtt (runMqtt, client, pub) where


-- Different string modules should be unified under one interface
import qualified Data.Text as Text 
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BSC



import qualified Network.MQTT.Client as MQ
import qualified Network.MQTT.Topic as MQ ()
import qualified Network.MQTT.Types as MQTy

import Network.Connection
import Network.TLS
--import Data.X509.CertificateStore ()
--import Data.X509.Validation (validateDefault)
import Data.Default.Class
import Network.TLS.Extra.Cipher
import Network.URI

import Control.Exception (Handler (..), catches)
import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM

import Data.ProtoLens (encodeMessage)

import Chopaan.Types (MQTTOpts(..))
import Chopaan.Comm.Comm (Address(..), Dispatch(..), NodeQueue(..))

-- I want to setup an MQTT client that subscribes to kibuttz/node/{mac}/state and publishes to /kibbutz/node/{mac}/control


-- https://stackoverflow.com/questions/40081508/how-to-provide-a-client-certificate-to-http-client-tls
mkTLSSettings :: FilePath -> FilePath -> FilePath -> Text.Text -> Text.Text -> IO TLSSettings
mkTLSSettings cert key caPath hostName name = do
  creds <- either (error "Client Certificate Not Found") Just <$> credentialLoadX509 cert key
  --caCreds <- fromJust (error "CA Certificate Not Found") (readCertificateStore caPath)
  let
    hooks = def { onCertificateRequest = \_ -> return creds
                , onServerCertificate = \_ a b c -> return [] --validateDefault caCreds a b c
                }
    clientParams = (defaultParamsClient (Text.unpack hostName :: HostName) ((BSC.pack . Text.unpack) name))
                  { clientHooks=hooks
                  , clientSupported = def {supportedCiphers=ciphersuite_strong}
                  }
  return (TLSSettings clientParams)


-- need reader for creds and logs
runMqtt :: forall a b. (Address a, Dispatch b) => MQTTOpts -> NodeQueue a b -> [a] -> MQ.MessageCallback -> IO ()
runMqtt opts outQueue ts msgCB = do
  mc <- client opts msgCB
  _ <- forkIO $ forever $ catches (pub mc outQueue) [Handler errorHandler]
  _ <- mapM (subscribe mc) ts -- [("/kibbutz/node/240ac4c662ac/state", MQ.subOptions)]
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
           , MQ._connectTimeout=1800000
           , MQ._tlsSettings=tlsConf}
  MQ.connectURI conf uri

subscribe :: (Address n) => MQ.MQTTClient -> n -> IO ([Either MQTy.SubErr MQ.QoS])
subscribe c n = fst <$> MQ.subscribe c [subTopic n] []

subTopic :: (Address n) => n -> (MQ.Topic, MQ.SubOptions)
subTopic n = (stateTopic n, MQ.subOptions)

-- The pub queue is a concurrent friendly data structure. We also probably want to put the client in one. But clients are
    -- not stateful.
pub :: forall a b. (Address a, Dispatch b) => MQ.MQTTClient -> NodeQueue a b -> IO ()
pub c tv = do
  forever $ pub' =<< (atomically $ do readTBQueue (runNodeQueue tv))
    where
      pub' :: (a, b) -> IO ()
      pub' (nId, mf) = --putStrLn ("Publishing Message for topic: " <> (show $ topic nId)) >>
        MQ.publish c (controlTopic nId) (encode mf) False
      encode = BL.fromStrict . encodeMessage . frame



errorHandler :: MQ.MQTTException -> IO ()
errorHandler (MQ.Timeout) = putStrLn ("ERROR : Timeout") >> threadDelay 100000
errorHandler (MQ.BadData) = putStrLn ("ERROR : BadData") >> threadDelay 100000
errorHandler (MQ.Discod d) = putStrLn ("ERROR Discod -> " <> (show d)) >> threadDelay 100000
errorHandler (MQ.MQTTException e) = putStrLn ("ERROR :" <> (show e)) >> threadDelay 100000
