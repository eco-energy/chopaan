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
import qualified Data.ByteString as B



import qualified Network.MQTT.Client as MQ
import qualified Network.MQTT.Topic as MQ ()
import qualified Network.MQTT.Types as MQTy

import Network.Connection
import Network.TLS
--import Data.X509.CertificateStore (readCertificateStore)
--import Data.X509.Validation (validateDefault)

import Data.Default.Class
import Network.TLS.Extra.Cipher
import Network.URI

import Control.Exception (Handler (..), catches)
import Control.Monad (forever, void)
import Control.Monad.IO.Class
import Control.Concurrent (forkIO)
import Control.Concurrent.STM

import Data.ProtoLens (encodeMessage)

import Chopaan.Kibbutz.AWS.Things (ThingCreds(..))
import Chopaan.Comm.Mqtt.AWS (MQTTCreds) --, withMqttAuth)
import Chopaan.Types (MQTTOpts(..))
import Chopaan.Comm.Comm (Address(..), PubQueue, initMessageQs, MessageQs(..))
import Chopaan.Comm.Queues (NodeQueue(..))
import Chopaan.Utils.Retry
import Proto.NodeMessageSchema.NodeMessages (MeshFrame)



-- I want to setup an MQTT client that subscribes to kibuttz/node/{mac}/state and publishes to /kibbutz/node/{mac}/control


-- https://stackoverflow.com/questions/40081508/how-to-provide-a-client-certificate-to-http-client-tls
mkTLSSettingsFromMemory :: B.ByteString -> B.ByteString -> B.ByteString -> Text.Text -> Text.Text -> TLSSettings
mkTLSSettingsFromMemory cert key caPath hostName name = let
  creds = either (const Nothing) (Just) (credentialLoadX509FromMemory cert key)
  --caCreds <- fromJust (error "CA Certificate Not Found") (readCertificateStore caPath)
  hooks = def { onCertificateRequest = \_ -> return creds
              , onServerCertificate = \_ _ _ _ -> mempty -- validateDefault caCreds a b c
              }
  clientParams = (defaultParamsClient (Text.unpack hostName :: HostName) ((BSC.pack . Text.unpack) name))
                 { clientHooks=hooks
                 , clientSupported = def {supportedCiphers=ciphersuite_strong}
                 }
  in (TLSSettings clientParams)

mkTLSSettingsFromDisk :: FilePath -> FilePath -> FilePath -> Text.Text -> Text.Text -> IO TLSSettings
mkTLSSettingsFromDisk cert key caPath hostName name = do
  creds <- either (const Nothing) (Just) <$> (credentialLoadX509 cert key)
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
runMqtt ::
  forall m a. (MonadIO m, Address a)
  => MQTTOpts
  -> [a]
  -> (MessageQs a -> MQ.MessageCallback)
  -> MQTTCreds
  -> m (MessageQs a)
runMqtt opts ts msgCB creds = do
  qs@MessageQs{..} <- liftIO initMessageQs
  mc <- liftIO $ client opts (msgCB qs) creds
  _ <- liftIO . forkIO $ forever $ catches (pub mc outbox) [(Handler errorHandler)]
  connStatus <- liftIO $ sequence $ (resub mc) <$> ts
  liftIO $ print connStatus
  void . liftIO . forkIO $ recoverC "waiting for client" 10 (MQ.waitForClient mc)
  return qs


client ::
  MQTTOpts
  -> MQ.MessageCallback
  -> MQTTCreds
  -> IO (MQ.MQTTClient)
client fileOpts msgCB awsCreds = do
  tlsConf <- return $ mkTLSSettingsFromMemory (cert awsCreds) (privateKey awsCreds) undefined (mqttURI fileOpts) (connId fileOpts)
  let
    (Just uri) = parseURI $ Text.unpack $ (mqttURI fileOpts) <> "#" <> (connId fileOpts)
    conf = MQ.mqttConfig
           { MQ._protocol=MQ.Protocol311
           , MQ._connID=Text.unpack $ (connId fileOpts)
           --, MQ._port=8883
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
  (\s -> recoverC (getMsgLog s) 10 (pub' s)) =<< (atomically $ do readTBQueue (runNodeQueue tv))
    where
      getMsgLog (nId, _) = "Message Publish: " <> show nId 
      pub' :: (MQ.Topic, MeshFrame) -> IO ()
      pub' (nId, mf) =
        MQ.publish c nId (encode mf) False
      encode = BL.fromStrict . encodeMessage

resub :: (Address n) => MQ.MQTTClient -> n -> IO (Either MQTy.SubErr MQ.QoS)
resub c n = retryEither n (subscribe c)


errorHandler :: MQ.MQTTException -> IO ()
errorHandler (MQ.Timeout) = printError "Timeout" 
errorHandler (MQ.BadData) = printError "BadData" 
errorHandler (MQ.Discod d) = printError d  
errorHandler (MQ.MQTTException e) =  printError e
printError :: Show e => e -> IO ()
printError e = print $ "MQTT Publisher Exception:\n" <> (show e)
