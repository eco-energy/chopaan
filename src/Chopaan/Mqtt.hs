{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings#-}

module Chopaan.Mqtt (runMqtt) where


-- Different string modules should be unified under one interface
import qualified Data.Text as Text 
import qualified Data.ByteString.Lazy as BL
--import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

--import qualified Data.Time as Time


import qualified Network.MQTT.Client as MQ
import qualified Network.MQTT.Topic as MQ ()
import qualified Network.MQTT.Types as MQTy ()
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

import Chopaan.Registry (NodeT, HasTopics(..), NodeQueue(..))

import Data.ProtoLens (encodeMessage, Message)
import Chopaan.Types (MQTTOpts(..))


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
runMqtt :: forall a b. (HasTopics a, Message b) => MQTTOpts -> NodeQueue NodeT b -> [a] -> MQ.MessageCallback -> IO ()
runMqtt MQTTOpts{..} outQueue ts msgCB = do
  tlsConf <- mkTLSSettings certPath keyPath caPath mqttURI connId
  let
    (Just uri) = parseURI $ Text.unpack $ mqttURI <> "#" <> connId
    conf = MQ.mqttConfig
           { MQ._protocol=MQ.Protocol311
           , MQ._connID=Text.unpack $ connId
           --, MQ._port=443
           , MQ._msgCB=msgCB
           , MQ._connectTimeout=180000000000
           , MQ._tlsSettings=tlsConf}
    topics = zip (map stateTopic ts) $ repeat MQ.subOptions --{MQ._retainHandling=MQTy.DoNotSendOnSubscribe,
                                                            --MQ._retainAsPublished=False,
                                                            --MQ._noLocal=True,
                                                            --MQ._subQoS=MQ.QoS0}
  -- TODO: Add a logging Error Handler
  mc <- MQ.connectURI conf uri
  _ <- forkIO $ forever $ catches (pub mc outQueue) [Handler handler]
  _ <- mapM (\t -> MQ.subscribe mc [t] []) topics -- [("/kibbutz/node/240ac4c662ac/state", MQ.subOptions)]
  MQ.waitForClient mc
  --_ <- forkIO $ forever $  catches (sub mc [head topics]) [Handler (\(ex :: IOException) -> putStrLn $ "IOError: " <> show ex)]
  where
    handler :: MQ.MQTTException -> IO ()
    handler (MQ.Timeout) = putStrLn ("ERROR : Timeout") >> threadDelay 100000
    handler (MQ.BadData) = putStrLn ("ERROR : BadData") >> threadDelay 100000
    handler (MQ.Discod d) = putStrLn ("ERROR Discod -> " <> (show d)) >> threadDelay 100000
    handler (MQ.MQTTException e) = putStrLn ("ERROR :" <> (show e)) >> threadDelay 100000

    -- The pub queue is a concurrent friendly data structure. We also probably want to put the client in one. But clients are
    -- not stateful.
    pub :: MQ.MQTTClient -> NodeQueue NodeT b -> IO ()
    pub c tv = do
      forever $ pub' =<< (atomically $ do readTBQueue (runNodeQueue tv))
      where
        pub' :: (NodeT, b) -> IO ()
        pub' (nId, mf) = --putStrLn ("Publishing Message for topic: " <> (show $ topic nId)) >>
          MQ.publish c (topic nId) (encode mf) False
        topic :: NodeT -> MQ.Topic
        topic = controlTopic
        encode :: b -> BL.ByteString
        encode = BL.fromStrict . encodeMessage
