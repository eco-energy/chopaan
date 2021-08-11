{-# LANGUAGE QuasiQuotes, NamedFieldPuns, OverloadedStrings  #-}
module Chopaan.Comm.Mqtt.AWS where


import qualified Data.Text as Text


import Control.Monad.Trans.AWS
import Control.Exception (bracket)
import Lens.Micro

import Text.InterpolatedString.Perl6

import qualified Network.AWS.IoT.DescribeEndpoint as DE
import Control.Monad.Trans.AWS ()
import Control.Monad.IO.Class

import Chopaan.Node.NodeId
import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.AWS.Things
import Chopaan.Kibbutz.AWS.Common

type ChopaanId = Text.Text
type MQTTCreds = ThingCreds


--defId :: ChopaanId
--defId = "chopaan-v1"

chopaanId :: KbtzName -> ThingName
chopaanId (KbtzId k) = k

withMqttAuth :: Logger -> KbtzName -> (ThingCreds -> IO c) -> IO c
withMqttAuth lgr k f = do
  print "Running MQTT With Auth"
  bracket
    (registerChopaanIO lgr k)
    (deregisterChopaanIO lgr k)
    f
  

registerChopaanIO :: Logger -> KbtzName -> IO (MQTTCreds)
registerChopaanIO lgr k = ((inIotContext lgr) . registerChopaan $ k)

deregisterChopaanIO :: Logger -> KbtzName -> MQTTCreds -> IO Bool
deregisterChopaanIO lgr k = ((inIotContext lgr) . (deregisterChopaan k))


registerChopaan :: KbtzName -> AWSC (ThingCreds)
registerChopaan k = createCertAndKey >>= (\mc@ThingCreds{certId} -> do
                                            liftIO . print $ "Registed Certificate"
                                            t <- registerThing k (chopaanId k) certId chopaanTemplate
                                            liftIO . print $ "Registered Thing" -- <> (show t)
                                            return mc)


deregisterChopaan :: KbtzName -> ThingCreds -> AWSC (Bool)
deregisterChopaan k ThingCreds{certARN, certId} = do
  liftIO . print $ "Deregistering Chopaan"
  c <- detachCert (chopaanId k) certARN
  liftIO . print $ "Detached Certificate:" <> (show c)
  d <- deleteCert certId certARN
  liftIO . print $ "Deleting Certificate:" <> (show d)
  deleteThing (chopaanId k)


getIoTEndpoint :: AWSC (Maybe Text.Text)
getIoTEndpoint = do
  e <- send $ DE.describeEndpoint & DE.deEndpointType .~ (Just "iot:Data-ATS")
  liftIO . print $ "IOT Endpoint: " <> (show $ e ^. DE.dersEndpointAddress  )
  return $ e ^. DE.dersEndpointAddress 
  
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
