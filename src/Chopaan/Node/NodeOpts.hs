{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Chopaan.Node.NodeOpts where

import Dhall

data PVConfig = PVConfig
  { vOC :: !Double
  , vMPP :: !Double
  , iMPP :: !Double
  , power :: !Double } deriving (Eq, Generic, Show)

instance FromDhall PVConfig

data BatteryType = LAFlooded | LASealed | LIon deriving (Eq, Generic, Show, Enum)

instance FromDhall BatteryType

data BatteryConfig = BatteryConfig
  { _type :: !BatteryType
  , cutOffVoltage :: !Double
  , maxV :: !Double
  , ampHours :: !Double
  } deriving (Eq, Generic, Show)

instance FromDhall BatteryConfig

data NodeConfig = NodeConfig
  { macAddress :: !Text
  , battery :: ![BatteryConfig]
  , pv :: ![PVConfig]
  } deriving (Generic, Show)

instance FromDhall NodeConfig
