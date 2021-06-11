{-# LANGUAGE OverloadedStrings, KindSignatures, DataKinds, NamedFieldPuns, ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DeriveFoldable, DeriveTraversable
, DerivingStrategies, DeriveAnyClass, DeriveFunctor, StandaloneDeriving, DerivingVia #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses, TypeFamilies, FunctionalDependencies, ScopedTypeVariables #-}
{-# LANGUAGE CPP, TemplateHaskell #-}
module Chopaan.Node.HW where

#include "UpdateInst.inc"

import Control.Lens
import GHC.Generics

import Control.Monad.Except (MonadError (throwError))
import Control.DeepSeq (NFData)

import Data.Text
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Greskell (Key, lookupAs, lookupM, lookup, pMapToFail
                     , FromGraphSON(..), PMap(..)
                     , GValue, Single, Multi, Parser)
import Data.Greskell.GraphSON.GValue (unwrapAll)
import Data.Greskell.Extra (writeKeyValues, (<=:>))

import NetSpider.Found (FoundNode(..), FoundLink(..))
import NetSpider.Graph (LinkAttributes(..), NodeAttributes(..), VFoundNode, EFinds)
import Data.Monoid (Sum(..))

import Chopaan.Node.Components
import Chopaan.Graph.Greskell (GreskellC, parseUnwrapTraversable)

import Shpadoinkle.Widgets.Types (Field, Humanize (..)
                                 , Hygiene (Clean)
                                 , Input (Input)
                                 , Status (Edit, Errors, Valid)
                                 , Validate(..), Validated(..), Present
                                 , Pick (AtleastOne, One)
                                 , fullOptions
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
  } deriving (Eq, Ord, Show, Generic, NFData, Functor, Foldable, Traversable)

defHW :: Num a => HW a
defHW = HW (SingBC defBC) (SingPC defPC) (SingLC defLC)


instance (Show a) => Humanize (HW a)

storageKey :: (Num a) => Key VFoundNode (BatteryTop a)
storageKey = "hw_storage"

generationKey :: (Num a) => Key VFoundNode (PVTop a)
generationKey = "hw_generation"

loadKey :: (Num a) => Key VFoundNode (LoadTop a)
loadKey = "hw_load"

instance (GreskellC a, Num a, Show a, Read a) => NodeAttributes (HW a) where
  writeNodeAttributes hw = fmap writeKeyValues $ sequence $
    [ storageKey <=:> storage hw
    , generationKey <=:> generation hw
    , loadKey <=:> loads hw
    ]
  parseNodeAttributes props = pMapToFail (HW
                                          <$> lookupAs storageKey props
                                          <*> lookupAs generationKey props
                                          <*> lookupAs loadKey props
                                         )

instance (GreskellC a, Num a, Read a) => FromGraphSON (HW a) where
  -- parseGraphSON = parseUnwrapTraversable
  parseGraphSON gv = fromPMap =<< parseGraphSON gv
    where
      lookupAsF k pm = maybe (fail "key not found") parseGraphSON (Data.Greskell.lookup k pm)
      fromPMap :: PMap Single GValue -> Parser (HW a)
      fromPMap pm = do
        s <- (lookupAsF storageKey pm)
        g <- (lookupAsF generationKey pm)
        c <- (lookupAsF loadKey pm)
        return $ HW s g c
      


newtype WattHours = WattHours Double
  deriving stock (Generic)
  deriving newtype (Fractional, Real, Enum, Eq, Ord, Show, Read, Num, ToJSON, FromJSON)
  deriving anyclass (Humanize, Present, NFData)
  deriving (Semigroup, Monoid) via (Sum Double)

newtype Volts = Volts Double
  deriving stock (Generic)
  deriving newtype (Fractional, Real, Enum, Eq, Ord, Show, Read, Num, ToJSON, FromJSON)
  deriving anyclass (Humanize, Present, NFData)
  deriving (Semigroup, Monoid) via (Sum Double)

newtype Amperes = Amperes Double
  deriving stock (Generic)
  deriving newtype (Fractional, Real, Enum, Eq, Ord, Show, Read, Num, ToJSON, FromJSON)
  deriving anyclass (Humanize, Present, NFData)
  deriving (Semigroup, Monoid) via (Sum Double)

newtype Watts = Watts Double
  deriving stock (Generic)
  deriving newtype (Fractional, Real, Enum, Eq, Ord, Show, Read, Num, ToJSON, FromJSON)
  deriving anyclass (Humanize, Present, NFData)
  deriving (Semigroup, Monoid) via (Sum Double)

newtype Hours = Hours Double
  deriving stock (Generic)
  deriving newtype (Fractional, Real, Enum, Eq, Ord, Show, Read, Num, ToJSON, FromJSON)
  deriving anyclass (Humanize, Present, NFData)
  deriving (Semigroup, Monoid) via (Sum Double)

data StorageUpdate (s :: Status) = StorageUpdate
  { _capacity :: Field s Text Input WattHours
  , _minVoltage :: Field s Text Input Volts
  , _maxVoltage :: Field s Text Input Volts
  , _batteryType :: Field s Text (Dropdown 'One) BatteryType
  } deriving (Generic)

UpdateInstances(StorageUpdate)

instance ( NFData (Field s Text Input (WattHours))
         , NFData (Field s Text Input (Volts))
         , NFData (Field s Text Input (Watts))
         , NFData (Field s Text (Dropdown 'One) (BatteryType))
         ) => NFData (StorageUpdate s)

instance Validate StorageUpdate where
  rules = StorageUpdate
    { _capacity = positive
    , _minVoltage = positive
    , _maxVoltage = positive
    , _batteryType = maybe (throwError "Cannot be empty") pure
    }

storageForm :: StorageUpdate 'Edit
storageForm = StorageUpdate
    { _capacity = Input Clean 0
    , _minVoltage = Input Clean 0
    , _maxVoltage = Input Clean 0
    , _batteryType = fullOptions
    }

makeFieldsNoPrefix ''StorageUpdate

data GenerationUpdate (s :: Status) = GenerationUpdate
  { _genPower :: Field s Text Input Watts
  , _openCircuitVoltage :: Field s Text Input Volts
  , _voltageAtMPP :: Field s Text Input Volts
  , _currentAtMPP :: Field s Text Input Amperes 
  } deriving (Generic)

instance ( NFData (Field s Text Input (Amperes))
         , NFData (Field s Text Input (Volts))
         , NFData (Field s Text Input (Watts))
         ) => NFData (GenerationUpdate s)

UpdateInstances(GenerationUpdate)

instance Validate GenerationUpdate where
  rules = GenerationUpdate
    { _genPower = positive
    , _openCircuitVoltage = positive
    , _voltageAtMPP = positive
    , _currentAtMPP = positive
    }

generationForm :: GenerationUpdate 'Edit
generationForm = GenerationUpdate
  { _genPower = Input Clean 0
  , _openCircuitVoltage = Input Clean 0
  , _voltageAtMPP = Input Clean 0
  , _currentAtMPP = Input Clean 0 
  }

makeFieldsNoPrefix ''GenerationUpdate

data LoadUpdate (s :: Status) = LoadUpdate
  { _loadPowerU :: Field s Text Input Watts
  , _loadDuration :: Field s Text Input Hours
  } deriving (Generic)

makeFieldsNoPrefix ''LoadUpdate

instance ( NFData (Field s Text Input (Watts))
         , NFData (Field s Text Input (Hours))
         ) => NFData (LoadUpdate s)

UpdateInstances(LoadUpdate)

instance Validate LoadUpdate where
  rules = LoadUpdate { _loadPowerU = positive
                     , _loadDuration = positive
                     }

loadForm :: LoadUpdate 'Edit
loadForm = LoadUpdate
  { _loadPowerU = Input Clean 0
  , _loadDuration = Input Clean 0
  }

data HWUpdate (s :: Status) = HWUpdate
  { _storageU :: StorageUpdate s
  , _generationU :: GenerationUpdate s
  , _loadU :: LoadUpdate s
  } deriving (Generic)

makeFieldsNoPrefix ''HWUpdate



type NFDataHW s = (NFData (Field s Text Input (WattHours))
         , NFData (Field s Text Input (Amperes))
         , NFData (Field s Text Input (Volts))
         , NFData (Field s Text Input (Watts))
         , NFData (Field s Text Input (Hours))
         , NFData (Field s Text (Dropdown 'One) (BatteryType)))

instance ( NFDataHW s
         ) => NFData (HWUpdate s)

UpdateInstances(HWUpdate)

instance Validate HWUpdate where
  rules = HWUpdate { _storageU = rules, _generationU = rules, _loadU = rules }
  validate (HWUpdate{_storageU, _generationU, _loadU}) = HWUpdate
    { _storageU = validate _storageU
    , _generationU = validate _generationU
    , _loadU = validate _loadU
    }
  getValid (HWUpdate{_storageU,_generationU, _loadU}) = case getValid _storageU of
    Nothing -> Nothing
    Just x -> case getValid _generationU of
      Nothing -> Nothing
      Just y -> case getValid _loadU of
        Nothing -> Nothing
        Just z -> Just (HWUpdate { _storageU = x
                                 , _generationU = y
                                 , _loadU = z})

emptyHWForm :: HWUpdate 'Edit
emptyHWForm = HWUpdate
  { _storageU = storageForm
  , _generationU = generationForm
  , _loadU = loadForm
  }



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
