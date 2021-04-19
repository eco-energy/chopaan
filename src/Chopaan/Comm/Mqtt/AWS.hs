{-# LANGUAGE QuasiQuotes, NamedFieldPuns, OverloadedStrings  #-}
module Chopaan.Comm.Mqtt.AWS where

import Chopaan.Node.NodeId
import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.AWS.Things

import qualified Data.Text as Text


import Control.Monad.Trans.AWS
import Control.Exception (bracket)


import Text.InterpolatedString.Perl6





type ChopaanId = Text.Text
type MQTTCreds = ThingCreds


defId :: ChopaanId
defId = "chopaan-v1"

chopaanId :: KbtzName -> ThingName
chopaanId (KbtzId k) = k

withMqttAuth :: Logger -> KbtzName -> (ThingCreds -> IO c) -> IO c
withMqttAuth lgr k = bracket
  (registerChopaanIO lgr k)
  (deregisterChopaanIO lgr k)
  

registerChopaanIO :: Logger -> KbtzName -> IO (MQTTCreds)
registerChopaanIO lgr k = ((inIotContext lgr) . registerChopaan $ k)

deregisterChopaanIO :: Logger -> KbtzName -> MQTTCreds -> IO Bool
deregisterChopaanIO lgr k = ((inIotContext lgr) . (deregisterChopaan k))


registerChopaan :: KbtzName -> AWSC (ThingCreds)
registerChopaan k = createCertAndKey >>= (\mc@ThingCreds{certId} ->
                                              registerThing k (chopaanId k) certId chopaanTemplate
                                              >> return mc)


deregisterChopaan :: KbtzName -> ThingCreds -> AWSC (Bool)
deregisterChopaan k ThingCreds{certARN, certId} = do
  detachCert (chopaanId k) certARN
  deleteCert certId certARN
  deleteThing (chopaanId k)



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
