{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, NamedFieldPuns, TypeApplications #-}
module Chopaan.Kibbutz.AWS.Things where

import Chopaan.Kibbutz.KbtzId
import Chopaan.Node.NodeId
import Chopaan.Utils.Retry

import Data.HashMap.Strict
import Data.Aeson (fromJSON)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as B

import qualified Network.MQTT.Topic as MQ

import Lens.Micro

-- AWS Imports
import qualified Network.AWS.IoT.ListThings as Thing
import qualified Network.AWS.IoT.RegisterThing as Thing
import qualified Network.AWS.IoT.DeleteThing as Thing

import qualified Network.AWS.IoT.Types as Iot
import qualified Network.AWS.IoT.DescribeCertificate as Cert
import qualified Network.AWS.IoT.CreateKeysAndCertificate as Cert
import qualified Network.AWS.IoT.UpdateCertificate as Cert
import qualified Network.AWS.IoT.DeleteCertificate as Cert
import qualified Network.AWS.IoT.DetachPolicy as Policy

import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource
import Control.Exception (bracket)

import Data.Maybe
import System.IO

import Text.InterpolatedString.Perl6

-- Streamly
import Streamly ()
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Unfold as UF
import qualified Streamly.Internal.Data.Unfold.Types as UF
import qualified Streamly.Internal.Data.Stream.StreamD.Type as STy

nameToTopic :: Text.Text -> ThingName -> MQ.Topic
nameToTopic suffix name = prefix <> n <> suffix
  where
    prefix = "/kibbutz/node/"
    n = Text.replace ":" "" name

topicToNodeId :: Text.Text -> Text.Text -> Maybe NodeMAC
topicToNodeId suffix t =
  case (isValidTopic t) of
       False  -> Nothing
       _ -> Just (NodeId n)
  where
    n = colonize $ Text.replace suffix "" $ Text.replace prefix "" t
    isValidTopic t' = prefix `Text.isPrefixOf` t' && suffix `Text.isSuffixOf` t'
    prefix = "/kibbutz/node/"
    colonize :: Text.Text -> Text.Text
    colonize cs = Text.intercalate i $ Text.chunksOf 2 cs
      where
        i = ":"

{--------------------------------------------------------------------------------------------------------

                   Thing Tings and Rules for Topics
---------------------------------------------------------------------------------------------------------}

success :: Int -> Bool
success = (== 200)
{--
retryOnFail :: (MonadIO m) => m a -> (a -> Int) -> (a -> b) -> m b
retryOnFail action getStatus getRes = do
  r <- action
  case (success . getStatus $ r) of
    True -> return . getRes $ r
    False -> retryBool action
--}

iotApi :: Service
iotApi = iot "execute-api"

type AWSC b = AWST' Env (ResourceT IO) b

inAwsContext :: AWST' Env (ResourceT IO) b -> IO b
inAwsContext ma = do
  lgr <- newLogger Debug stdout
  env <- newEnv Discover <&> set envLogger lgr . set envRegion Singapore <&> configure iotApi  
  runResourceT . runAWST env $ ma

thingName :: Iot.ThingAttribute -> Maybe ThingName
thingName t = t ^. Iot.taThingName

iot :: B.ByteString -> Service
iot svc = Iot.ioT{_svcPrefix=svc} :: Service

getThings :: Text.Text -> AWSC [Iot.ThingAttribute]
getThings thingTypeName = do
  let
    req = (Thing.listThings & Thing.ltThingTypeName .~ (Just thingTypeName))
  things <- S.toList
    $ S.map (\x -> x ^. Thing.ltrsThings)
    $ S.unfold pageUF req
  return $ concat things


getCertPem :: CertId -> AWSC (Maybe Text.Text)
getCertPem certId = do
  c <- send $ Cert.describeCertificate certId
  return $ c ^? Cert.dcrsCertificateDescription . _Just . Iot.cdCertificatePem . _Just


type ChopaanId = Text.Text

defId :: ChopaanId
defId = "chopaan-v1"

data MQTTCreds = MQTTCreds
  { certId :: CertId
  , cert :: B.ByteString
  , privateKey :: B.ByteString
  , certARN :: CertARN
  } deriving (Eq, Ord, Show)

type CertId = Text.Text
type CertARN = Text.Text

thingMap :: ThingName -> KbtzName -> CertId -> HashMap Text.Text Text.Text
thingMap thing (KbtzId kbtz) cert = fromList $ [("ThingName", thing)
                           , ("CertificateId", cert)
                           , ("CommonName", thing)
                           , ("Kibbutz", kbtz)]

chopaanId :: KbtzName -> ThingName
chopaanId (KbtzId k) = "chopaan-" <> k

withMqttAuth :: KbtzName -> (MQTTCreds -> IO c) -> IO c
withMqttAuth k = bracket
  (inAwsContext . registerChopaan $ k)
  (inAwsContext . (deregisterChopaan k))

registerChopaan :: KbtzName -> AWSC (MQTTCreds)
registerChopaan k = createCertAndKey >>= (\mc@MQTTCreds{certId} ->
                                              registerThing k (chopaanId k) certId
                                              >> return mc)


deregisterChopaan :: KbtzName -> MQTTCreds -> AWSC (Bool)
deregisterChopaan k MQTTCreds{certARN, certId} = deleteCert certId certARN
  >> deleteThing (chopaanId k)  

createCertAndKey :: AWSC (MQTTCreds)
createCertAndKey = do
  let req = Cert.createKeysAndCertificate & Cert.ckacSetAsActive .~ (Just True)
  c <- send req
  return $ MQTTCreds
    { certId = fromJust $ c ^. Cert.ckacrsCertificateId
    , cert = encodeUtf8 . fromJust $ c ^. Cert.ckacrsCertificatePem
    , privateKey = encodeUtf8 . fromJust $ c ^? Cert.ckacrsKeyPair . _Just . Iot.kpPrivateKey . _Just
    , certARN = fromJust (c ^. Cert.ckacrsCertificateARN)
    }


registerThing :: KbtzName -> ThingName -> CertId -> AWSC (HashMap Text.Text Text.Text)
registerThing kbtz thing certId = do
  let req = Thing.registerThing chopaanTemplate
        & Thing.rtParameters .~ thingMap thing kbtz certId
  c <- send req
  return $ c ^. Thing.rtrsResourceARNs

deleteThing :: ThingName -> AWSC (Bool)
deleteThing thing = do
  c <- send $ Thing.deleteThing thing
  return $ success (c ^. Thing.ddrsResponseStatus)

deleteCert :: CertId -> CertARN  -> AWSC ()
deleteCert certId certArn = do
  send $ Policy.detachPolicy chopaanPolicy certArn
  send $ Cert.updateCertificate certId Iot.CSInactive
  send $ Cert.deleteCertificate certId
  return ()
  where
    chopaanPolicy = "kibbutz-node-comm"


pageUF :: forall m a r. (AWSPager a, AWSConstraint r m) => UF.Unfold m a (Rs a)
pageUF = UF.Unfold step inject
  where
    step :: Maybe a -> m (STy.Step (Maybe a) (Rs a)) 
    step (Just req) = do
      y <- send req
      return $ STy.Yield y (page req y)
    step Nothing = do
      return $ STy.Stop
    inject :: a -> m (Maybe a)
    inject = pure . Just


chopaanTemplate :: Text.Text
chopaanTemplate = [q|
{
    "Parameters" : {
        "ThingName" : {
            "Type" : "String"
        },
        "CommonName" : {
            "Type" : "String"
        },
        "Kibbutz" : {
            "Type" : "String",
            "Default" : "Test"
        },
        "CertificateId" : {
            "Type" : "String"
        }
    },
    "Resources" : {
        "thing" : {
            "Type" : "AWS::IoT::Thing",
            "Properties" : {
                "ThingName" : {"Ref" : "ThingName"},
                "AttributePayload" : { "version" : "v1", "kibbutz": {"Ref" : "Kibbutz"}, "commonName" :  {"Ref" : "CommonName"}},
                "ThingTypeName" :  "kibbutz-pilot-chopaan",
                "ThingGroups" : ["kibbutz-pilot-v1"]
            }
        },
        "certificate" : {
            "Type" : "AWS::IoT::Certificate",
            "Properties" : {
                "CertificateId": {"Ref" : "CertificateId"}
            },
            "OverrideSettings" : {
                "Status" : "DO_NOTHING"
            }
        },
        "policy" : {
            "Type" : "AWS::IoT::Policy",
            "Properties" : {
                "PolicyName" : "kibbutz-node-comm"
            }
        }
    }
}
|]
