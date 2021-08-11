{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, NamedFieldPuns, TypeApplications #-}
module Chopaan.Kibbutz.AWS.Things where


import Chopaan.Node.NodeId
import Chopaan.Kibbutz.KbtzId

import Chopaan.Kibbutz.AWS.Common

import qualified Data.Text as Text

import qualified Data.ByteString as B

import qualified Network.MQTT.Topic as MQ

import Lens.Micro

-- AWS Imports
import qualified Network.AWS.IoT.ListThings as Thing
import qualified Network.AWS.IoT.RegisterThing as Thing
import qualified Network.AWS.IoT.DeleteThing as Thing
import qualified Network.AWS.IoT.DetachThingPrincipal as Thing

import qualified Network.AWS.IoT.Types as Iot
import qualified Network.AWS.IoT.DescribeCertificate as Cert
import qualified Network.AWS.IoT.CreateKeysAndCertificate as Cert
import qualified Network.AWS.IoT.UpdateCertificate as Cert
import qualified Network.AWS.IoT.DeleteCertificate as Cert
import qualified Network.AWS.IoT.DetachPolicy as Policy

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource


import Data.Maybe
import Data.HashMap.Strict
import Data.Text.Encoding (encodeUtf8)


-- Streamly
import qualified Streamly.Prelude as S


{--------------- Name to Topic -------------------}

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

type CertId = Text.Text
type CertARN = Text.Text
type RoleTemplate = Text.Text

data ThingCreds = ThingCreds
  { certId :: CertId
  , cert :: B.ByteString
  , privateKey :: B.ByteString
  , certARN :: CertARN
  } deriving (Eq, Ord, Show)


success :: Int -> Bool
success = (== 200)

iotApi :: Service
iotApi = iot "execute-api"



inIotContext :: Logger -> AWST' Env (ResourceT IO) b -> IO b
inIotContext lgr = inAwsContext lgr iotApi

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

thingMap :: ThingName -> KbtzName -> CertId -> HashMap Text.Text Text.Text
thingMap thing (KbtzId kbtz) cert = fromList $ [("ThingName", thing)
                           , ("CertificateId", cert)
                           , ("CommonName", thing)
                           , ("Kibbutz", kbtz)]


createCertAndKey :: AWSC (ThingCreds)
createCertAndKey = do
  let req = Cert.createKeysAndCertificate & Cert.ckacSetAsActive .~ (Just True)
  c <- send req
  return $ ThingCreds
    { certId = fromJust $ c ^. Cert.ckacrsCertificateId
    , cert =  toPEM . fromJust $ c ^. Cert.ckacrsCertificatePem
    , privateKey = toPEM . fromJust $ c ^? Cert.ckacrsKeyPair . _Just . Iot.kpPrivateKey . _Just
    , certARN = fromJust (c ^. Cert.ckacrsCertificateARN)
    }
    where
      toPEM = encodeUtf8




detachCert :: ThingName -> CertARN -> AWSC ()
detachCert thing certArn = do
  void $ send $ Thing.detachThingPrincipal thing certArn

registerThing :: KbtzName -> ThingName -> CertId -> RoleTemplate -> AWSC (HashMap Text.Text Text.Text)
registerThing kbtz thing certId roleTemplate = do
  let req = Thing.registerThing roleTemplate
        & Thing.rtParameters .~ thingMap thing kbtz certId
  c <- send req
  --liftIO . print $ "Thing Registration: " <> (show c)
  return $ c ^. Thing.rtrsResourceARNs

deleteThing :: ThingName -> AWSC (Bool)
deleteThing thing = do
  c <- send $ Thing.deleteThing thing
  
  return $ success (c ^. Thing.ddrsResponseStatus)

deleteCert :: CertId -> CertARN  -> AWSC ()
deleteCert certId certArn = do
  p <- send $ Policy.detachPolicy chopaanPolicy certArn
  liftIO . print $ "Detaching Policy: " <> (show p)
  i <- send $ Cert.updateCertificate certId Iot.CSInactive
  liftIO . print $ "Detaching Policy: " <> (show i)
  d <- send $ Cert.deleteCertificate certId
  liftIO . print $ "Certificate Deletion: " <> (show d)
  return ()
  where
    chopaanPolicy = "kibbutz-node-comm"
