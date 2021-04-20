{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Chopaan.Types where

import RIO
import RIO.Process

import Dhall

import Chopaan.Node.NodeOpts

data DBOpts = DBOpts
  { host :: !Text
  , port :: !Integer
  , database :: !Text
  , user :: !Text
  , password :: !Text
  } deriving (Eq, Ord, Show, Generic)

instance FromDhall DBOpts

data MQTTOpts = MQTTOpts
  { connId :: !Text
  , mqttURI :: !Text
  , certPath :: !FilePath
  , keyPath :: !FilePath
  , caPath :: !FilePath
  } deriving (Eq, Ord, Show, Generic)

data KibbutzOpts = KibbutzOpts
  { name :: !Text
  } deriving (Generic, Show)


instance FromDhall MQTTOpts
instance FromDhall KibbutzOpts

-- | Command line arguments
data Options = Options
  { logVerbose :: !Bool
  , mqttOpts :: !MQTTOpts
  , nodeOpts :: ![NodeConfig]
  , kibbutzOpts :: !KibbutzOpts
  , dbOpts :: !DBOpts
  } deriving (Generic, Show)

instance FromDhall Options

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
