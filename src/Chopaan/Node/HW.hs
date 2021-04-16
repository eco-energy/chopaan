{-# LANGUAGE OverloadedStrings, KindSignatures, DataKinds, NamedFieldPuns, ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving
, DerivingStrategies, DeriveAnyClass, DeriveFunctor, StandaloneDeriving, DerivingVia #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE CPP #-}
module Chopaan.Node.HW where

#include "UpdateInst.inc"

import GHC.Generics


import Data.Text
import Data.Aeson (ToJSON, FromJSON)
import Control.DeepSeq (NFData)
import Data.Greskell (Key, lookupAs, pMapToFail)
import Data.Greskell.Extra (writeKeyValues, (<=:>))

import NetSpider.Found (FoundNode(..), FoundLink(..))
import NetSpider.Graph (LinkAttributes(..), NodeAttributes(..), VFoundNode, EFinds)
import Data.Monoid (Sum(..))

import Chopaan.Node.Components

import Shpadoinkle.Widgets.Types (Field, Humanize (..)
                                 , Hygiene (Clean)
                                 , Input (Input)
                                 , Status (Edit, Errors, Valid)
                                 , Validate(..), Validated(..), Present
                                 , Pick (AtleastOne, One)
                                 )

import Shpadoinkle.Widgets.Form.Dropdown as Dropdown (Dropdown)


import Shpadoinkle.Widgets.Validation ( between
                                      , nonMEmpty
                                      , nonZero
                                      , positive)


data HW a = HW
  { storage :: BatteryTop a
  , generation :: PVTop a
  , loads :: LoadTop a
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, NFData, Functor)

defHW :: Num a => HW a
defHW = HW (SingBC defBC) (SingPC defPC) (SingLC defLC)


instance (Show a) => Humanize (HW a)

instance NodeAttributes (HW a) where
  writeNodeAttributes hw = fmap writeKeyValues $ sequence $
    [ 
    ]
  parseNodeAttributes = undefined --pMapToFail





newtype WattHours = WattHours Double
  deriving stock (Generic)
  deriving newtype (Real, Enum, Eq, Ord, Show, Read, Num, ToJSON, FromJSON)
  deriving anyclass (Humanize, Present, NFData)
  deriving (Semigroup, Monoid) via (Sum Double)

newtype Volts = Volts Double
  deriving stock (Generic)
  deriving newtype (Real, Enum, Eq, Ord, Show, Read, Num, ToJSON, FromJSON)
  deriving anyclass (Humanize, Present, NFData)
  deriving (Semigroup, Monoid) via (Sum Double)

newtype Amperes = Amperes Double
  deriving stock (Generic)
  deriving newtype (Real, Enum, Eq, Ord, Show, Read, Num, ToJSON, FromJSON)
  deriving anyclass (Humanize, Present, NFData)
  deriving (Semigroup, Monoid) via (Sum Double)

newtype Watts = Watts Double
  deriving stock (Generic)
  deriving newtype (Real, Enum, Eq, Ord, Show, Read, Num, ToJSON, FromJSON)
  deriving anyclass (Humanize, Present, NFData)
  deriving (Semigroup, Monoid) via (Sum Double)

newtype Hours = Hours Double
  deriving stock (Generic)
  deriving newtype (Real, Enum, Eq, Ord, Show, Read, Num, ToJSON, FromJSON)
  deriving anyclass (Humanize, Present, NFData)
  deriving (Semigroup, Monoid) via (Sum Double)

data StorageUpdate (s :: Status) = StorageUpdate
  { capacity :: Field s Text Input WattHours
  , minVoltage :: Field s Text Input Volts
  , maxVoltage :: Field s Text Input Volts
  , batteryType :: Field s Text (Dropdown 'AtleastOne) BatteryType
  } deriving (Generic)

UpdateInstances(StorageUpdate)

instance ( NFData (Field s Text Input (WattHours))
         , NFData (Field s Text Input (Volts))
         , NFData (Field s Text Input (Watts))
         , NFData (Field s Text (Dropdown 'AtleastOne) (BatteryType))
         ) => NFData (StorageUpdate s)

instance Validate StorageUpdate where
  rules = StorageUpdate
    { capacity = positive
    , minVoltage = positive
    , maxVoltage = positive
    , batteryType = pure
    }


data GenerationUpdate (s :: Status) = GenerationUpdate
  { genPower :: Field s Text Input Watts
  , openCircuitVoltage :: Field s Text Input Volts
  , voltageAtMPP :: Field s Text Input Volts
  , currentAtMPP :: Field s Text Input Amperes 
  } deriving (Generic)


instance ( NFData (Field s Text Input (Amperes))
         , NFData (Field s Text Input (Volts))
         , NFData (Field s Text Input (Watts))
         ) => NFData (GenerationUpdate s)

UpdateInstances(GenerationUpdate)

instance Validate GenerationUpdate where
  rules = GenerationUpdate
    { genPower = positive
    , openCircuitVoltage = positive
    , voltageAtMPP = positive
    , currentAtMPP = positive
    }

data LoadUpdate (s :: Status) = LoadUpdate
  { loadPowerU :: Field s Text Input Watts
  , loadDuration :: Field s Text Input Hours
  } deriving (Generic)

instance ( NFData (Field s Text Input (Watts))
         , NFData (Field s Text Input (Hours))
         ) => NFData (LoadUpdate s)

UpdateInstances(LoadUpdate)

instance Validate LoadUpdate where
  rules = LoadUpdate { loadPowerU = positive
                     , loadDuration = positive
                     }

data HWUpdate (s :: Status) = HWUpdate
  { storageU :: StorageUpdate s
  , generationU :: GenerationUpdate s
  , loadU :: LoadUpdate s
  } deriving (Generic)


type NFDataHW s = (NFData (Field s Text Input (WattHours))
         , NFData (Field s Text Input (Amperes))
         , NFData (Field s Text Input (Volts))
         , NFData (Field s Text Input (Watts))
         , NFData (Field s Text Input (Hours))
         , NFData (Field s Text (Dropdown 'AtleastOne) (BatteryType)))

instance ( NFDataHW s
         ) => NFData (HWUpdate s)

UpdateInstances(HWUpdate)

instance Validate HWUpdate where
  rules = HWUpdate { storageU = rules, generationU = rules, loadU = rules }
  validate (HWUpdate{storageU, generationU, loadU}) = HWUpdate
    { storageU = validate storageU, generationU = validate generationU, loadU = validate loadU }
  getValid (HWUpdate{storageU, generationU, loadU}) = case getValid storageU of
    Nothing -> Nothing
    Just x -> case getValid generationU of
      Nothing -> Nothing
      Just y -> case getValid loadU of
        Nothing -> Nothing
        Just z -> Just (HWUpdate {storageU = x, generationU = y, loadU = z})

emptyHWForm :: HWUpdate 'Edit
emptyHWForm = undefined

{--
distanceKey :: Key EFinds Double
distanceKey = "distance"

gaugeKey :: Key EFinds Double
gaugeKey = "gauge"

instance LinkAttributes Wire where
  writeLinkAttributes w = fmap writeKeyValues $ sequence $
    [ distanceKey <=:> distance w
    , gaugeKey <=:> gauge w
    ] 
  parseLinkAttributes props = pMapToFail (Wire
                                          <$> lookupAs distanceKey props
                                          <*> lookupAs gaugeKey props
                                         )
--}
