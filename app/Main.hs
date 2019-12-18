{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

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
import Network.Connection
import Network.TLS
import Data.X509.CertificateStore
import Data.Default.Class 
import Network.TLS.Extra.Cipher
import Network.URI
{--

eTR :: NM.EnergyTransactionRequest
eTR =
  defMessage
      & uuid .~ ("123456" :: Text)
      & dispatchedAt .~ (223123123 :: Word64)
      & powerInWatts .~ (100 :: Double)
      & durationInSeconds .~ (60*60 :: Word64)
      & direction .~ Outgoing


meshFrame :: NM.MeshFrame
meshFrame =
  defMessage
      & time .~ (3424234453 :: Word64)
      & transaction .~ eTR
attrName :: Maybe Text.Text
attrName = Just "kibbutz"

kbtz :: Maybe Text.Text
kbtz = Just "PILOT"

--}

-- I want to setup an MQTT client that subscribes to kibuttz/node/{mac}/state and publishes to /kibbutz/node/{mac}/control
-- We obtain the list of nodes for a particular kibbutz by using the kibbutz name and querying AWS Iot for all thing names
-- for things which belong to that kibbutz.


type StateTopic = Text.Text
type ControlTopic = Text.Text

type ThingName = Text.Text


getThings :: IO [Iot.ThingAttribute]
getThings = do
  let
    iiot = Iot.ioT{_svcPrefix="execute-api"} :: Service 
    ttn = (Just "kibbutz-pilot-node") :: Maybe Text.Text
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
    prefix = "kibbutz/node/"
    n = Text.replace ":" "" name


-- https://stackoverflow.com/questions/40081508/how-to-provide-a-client-certificate-to-http-client-tls
mkTLSSettings :: IO TLSSettings
mkTLSSettings = do
  creds <- either (error "couldn't read cert") Just <$> credentialLoadX509 cert key
  let
    hooks = def { onCertificateRequest = \_ -> return creds
                , onServerCertificate = \_ _ _ _ -> return []
                }
    clientParams = (defaultParamsClient hostName "")
                  { clientHooks=hooks
                  , clientSupported = def {supportedCiphers=ciphersuite_all}
                  }
  return (TLSSettings clientParams)
  where
    cert = "certs/chopaan.cert.pem"
    key = "certs/chopaan.private.key.pem"
    hostName = "thisguy"


main :: IO ()
main = do
  things <- getThings
  tlsConf <- mkTLSSettings
  let
    topics = (fmap nameToTopics) <$> map thingName things
    stopics = zip (filter (\t -> t == "NoTopic") $ map ((fromMaybe "NoTopic") . fmap fst) topics) (repeat MQ.subOptions)
    (Just uri) = parseURI "mqtts://a1e7lyi19kctcn-ats.iot.ap-southeast-1.amazonaws.com"
    conf = MQ.mqttConfig
           { MQ._protocol=MQ.Protocol311
           , MQ._msgCB=MQ.SimpleCallback cb
           , MQ._tlsSettings=tlsConf}
  mc <- MQ.connectURI conf uri
  print =<< MQ.subscribe mc stopics []
  MQ.waitForClient mc
  where
    cb _ t m p =  print (t, m, p)
  
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
