{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Chopaan.Types where

import RIO
import RIO.Process

import Dhall

import Chopaan.NodeOpts

data MQTTOpts = MQTTOpts
  { connId :: !Text
  , mqttURI :: !Text
  , certPath :: !FilePath
  , keyPath :: !FilePath
  , caPath :: !FilePath
  } deriving (Eq, Ord, Show, Generic)

defMQOpts :: MQTTOpts
defMQOpts = MQTTOpts
  { connId = "chopaan-pilot-1"
  , mqttURI = "mqtts://a1e7lyi19kctcn-ats.iot.ap-southeast-1.amazonaws.com"
  , certPath = "certs/chopaan.cert.pem"
  , keyPath = "certs/chopaan.private.key.pem"
  , caPath = "certs/ca.cert.pem"
  }

data KibbutzOpts = KibbutzOpts
  { name :: !Text
  } deriving (Generic, Show)


instance Interpret MQTTOpts
instance Interpret KibbutzOpts

-- | Command line arguments
data Options = Options
  { logVerbose :: !Bool
  , mqttOpts :: !MQTTOpts
  , nodeOpts :: ![NodeConfig]
  , kibbutzOpts :: KibbutzOpts
  } deriving (Generic)

instance Interpret Options


data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
