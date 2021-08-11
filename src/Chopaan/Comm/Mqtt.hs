{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, RankNTypes #-}
{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeOperators #-}

module Chopaan.Comm.Mqtt (runMqtt, client, pub, MQ.Topic, MonadMQ, runMQ, pubQ) where

-- Different string modules should be unified under one interface
import qualified Data.Text as Text 
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as B

import Control.Monad.Trans.Reader


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

import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.AWS.Things (ThingCreds(..))
import Chopaan.Comm.Mqtt.AWS (MQTTCreds) --, withMqttAuth)
import Chopaan.Types (MQTTOpts(..))
import Chopaan.Comm.Comm (Address(..), PubQueue, initMessageQs, MessageQs(..))
import Chopaan.Comm.Queues (NodeQueue(..))
import Chopaan.Utils.Retry
import Chopaan.Graph.Spider
import Proto.NodeMessageSchema.NodeMessages (MeshFrame)
import Streamly as S
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL


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


runMqtt ::
  forall m n. (MonadIO m, Address n)
  => KbtzName
  -> [n]
  -> MessageQs n
  -> (MessageQs n -> MQ.MessageCallback)
  -> MQTTOpts
  -> MQTTCreds
  -> m ()
runMqtt kbtz ns qs@MessageQs{..} msgCB opts creds = do
  c <- liftIO $ client kbtz opts (msgCB qs) creds
  liftIO $ print ("Obtained Client!")
  _ <- liftIO . forkIO $ runMQ c (forever $ pubQ outbox)
  liftIO . (recoverC "waiting for client" 10) . runMQ c $ (runMqtt' ns outbox)

-- need reader for creds and logs
runMqtt' :: forall m a. (MonadIO m, Address a) => [a] -> PubQueue -> MonadMQ m ()
runMqtt' ts outbox = do
  -- liftIO . forkIO $ forever $ catches (runReaderT mc) [(Handler errorHandler)]
  connStatus <- resub ts
  liftIO $ print "Connection Status!"
  liftIO $ print connStatus
  (liftIO . MQ.waitForClient) =<< ask


client ::
  KbtzName
  -> MQTTOpts
  -> MQ.MessageCallback
  -> MQTTCreds
  -> IO (MQ.MQTTClient)
client (KbtzId k) fileOpts msgCB awsCreds = do
  --liftIO . print $ (fileOpts, awsCreds)
  tlsConf <- return $ mkTLSSettingsFromMemory (cert awsCreds) (privateKey awsCreds) undefined (mqttURI fileOpts) k
  let
    (Just uri) = parseURI $ Text.unpack $ (mqttURI fileOpts) <> "#" <> k
    conf = MQ.mqttConfig
           { MQ._protocol=MQ.Protocol311
           , MQ._connID=Text.unpack $ k
           --, MQ._port=8883
           , MQ._msgCB=msgCB
           , MQ._connectTimeout=18000000
           , MQ._tlsSettings=tlsConf}
  --print $ show conf
  recoverC "connectURI Attempting" 100000 $ MQ.connectURI conf uri

resub :: (MonadIO m, Address n) => [n] -> MonadMQ m [(Either MQTy.SubErr MQ.QoS)]
resub ns = (\c -> liftIO $ (subscribe c ns)) =<< ask

subscribe :: (Address n) => MQ.MQTTClient -> [n] -> IO [(Either MQTy.SubErr MQ.QoS)]
subscribe c ns = fst <$> (MQ.subscribe c (subTopic <$> ns) [])

subTopic :: (Address n) => n -> (MQ.Topic, MQ.SubOptions)
subTopic n = (stateTopic n, MQ.subOptions { MQ._subQoS = MQ.QoS1 })

-- The pub queue is a concurrent friendly data structure. We also probably want to put the client in one. But clients are
    -- not stateful.
pubQ :: (MonadIO m) => PubQueue -> MonadMQ m ()
pubQ tv = do
  pub =<< (liftIO . atomically $ do readTBQueue (runNodeQueue tv))

type MonadMQ m = ReaderT MQ.MQTTClient m

runMQ :: MQ.MQTTClient -> MonadMQ m ~> m
runMQ c a = runReaderT a c 

pub :: (MonadIO m) => (MQ.Topic, MeshFrame) -> MonadMQ m ()
pub s = do
  c <- ask
  liftIO $ recoverC (getMsgLog s) 10 (pub' c s)
    where
      getMsgLog (nId, _) = False 
      pub' :: MQ.MQTTClient -> (MQ.Topic, MeshFrame) -> IO ()
      pub' c (nId, mf) =
        liftIO $ MQ.publish c nId (encode mf) False
      encode = BL.fromStrict . encodeMessage


errorHandler :: MQ.MQTTException -> IO ()
errorHandler (MQ.Timeout) = printError "Timeout" 
errorHandler (MQ.BadData) = printError "BadData" 
errorHandler (MQ.Discod d) = printError d  
errorHandler (MQ.MQTTException e) =  printError e
printError :: Show e => e -> IO ()
printError e = print $ "MQTT Publisher Exception:\n" <> (show e)



