{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Chopaan.NodeOpts where

import Dhall

data PVConfig = PVConfig
  { vOC :: !Double
  , vMPP :: !Double
  , iMPP :: !Double
  , power :: !Double } deriving (Eq, Generic, Show)

instance Interpret PVConfig

data BatteryType = LAFlooded | LASealed | LIon deriving (Eq, Generic, Show, Enum)

instance Interpret BatteryType

data BatteryConfig = BatteryConfig
  { _type :: !BatteryType
  , cutOffVoltage :: !Double
  , maxV :: !Double
  , ampHours :: !Double
  } deriving (Eq, Generic, Show)

instance Interpret BatteryConfig

data NodeConfig = NodeConfig
  { macAddress :: !Text
  , battery :: ![BatteryConfig]
  , pv :: ![PVConfig]
  } deriving (Generic, Show)

instance Interpret NodeConfig
