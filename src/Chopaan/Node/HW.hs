{-# LANGUAGE OverloadedStrings, KindSignatures, DataKinds, NamedFieldPuns, ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DeriveFoldable, DeriveTraversable
, DerivingStrategies, DeriveAnyClass, DeriveFunctor, StandaloneDeriving, DerivingVia #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, MultiParamTypeClasses, TypeFamilies, FunctionalDependencies, ScopedTypeVariables #-}
{-# LANGUAGE CPP, TypeApplications, OverloadedLabels #-}
module Chopaan.Node.HW where

#include "UpdateInst.inc"

import Control.Lens
import GHC.Generics
import Data.Generics.Product
import Data.Generics.Labels ()


import Control.Monad.Except (MonadError (throwError))
import Control.DeepSeq (NFData)

import Data.Text
import qualified Codec.Winery as W
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Greskell (Key, lookupAs, lookupM, lookup, pMapToFail
                     , FromGraphSON(..), PMap(..)
                     , GValue, Single, Multi, Parser)
import Data.Greskell.GraphSON.GValue (unwrapAll)
import Data.Greskell.Extra (writeKeyValues, (<=:>))

#ifndef ghcjs_HOST_OS
import NetSpider.Graph (LinkAttributes(..), NodeAttributes(..), VFoundNode, EFinds)
#endif

import Data.Monoid (Sum(..))

import Chopaan.Node.Components
import Chopaan.Graph.Greskell

-- import Shpadoinkle (MonadJSM, Html)
-- import qualified Shpadoinkle.Html as H

-- -- import Shpadoinkle.Widgets.Types (Field (..)
--                                  , Hygiene (Clean)
--                                  , Input (Input)
--                                  , Status (Edit, Errors, Valid)
--                                  , Validate(..), Validated(..), Present
--                                  , Pick (AtleastOne, One)
--                                  , fullOptions
--                                  )

-- import Shpadoinkle.Widgets.Form.Dropdown as Dropdown (Dropdown)


-- import Shpadoinkle.Widgets.Validation ( between
                                      -- , nonMEmpty
                                      -- , nonZero
                                      -- , positive)


data HW a = HW
  { storage :: BatteryTop a
  , generation :: PVTop a
  , loads :: LoadTop a
  }
  deriving (Eq, Ord, Show, Generic, NFData, Functor, Foldable, Traversable)
  deriving W.Serialise via (W.WineryRecord (HW a))

instance (W.Serialise a) => ToJSON (HW a) where
  toJSON = wineryJSONWrite --genericToJSON pvEncodingOpts
  toEncoding = wineryJSONEncode

instance (W.Serialise a) => FromJSON (HW a) where
  parseJSON = wineryJSONRead "HW"



defHW :: Num a => HW a
defHW = HW (SingBC defBC) (SingPC defPC) (SingLC defLC)


#ifndef ghcjs_HOST_OS
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
#endif


newtype WattHours = WattHours Double
  deriving stock (Generic)
  deriving newtype (Fractional, Real, Enum, Eq, Ord, Show, Read, Num, ToJSON, FromJSON)
  deriving anyclass (NFData)
  deriving (Semigroup, Monoid) via (Sum Double)

newtype Volts = Volts Double
  deriving stock (Generic)
  deriving newtype (Fractional, Real, Enum, Eq, Ord, Show, Read, Num, ToJSON, FromJSON)
  deriving anyclass (NFData)
  deriving (Semigroup, Monoid) via (Sum Double)

newtype Amperes = Amperes Double
  deriving stock (Generic)
  deriving newtype (Fractional, Real, Enum, Eq, Ord, Show, Read, Num, ToJSON, FromJSON)
  deriving anyclass (NFData)
  deriving (Semigroup, Monoid) via (Sum Double)

newtype Watts = Watts Double
  deriving stock (Generic)
  deriving newtype (Fractional, Real, Enum, Eq, Ord, Show, Read, Num, ToJSON, FromJSON)
  deriving anyclass (NFData)
  deriving (Semigroup, Monoid) via (Sum Double)

newtype Hours = Hours Double
  deriving stock (Generic)
  deriving newtype (Fractional, Real, Enum, Eq, Ord, Show, Read, Num, ToJSON, FromJSON)
  deriving anyclass (NFData)
  deriving (Semigroup, Monoid) via (Sum Double)

-- data StorageUpdate (s :: Status) = StorageUpdate
--   { capacity :: Field s Text Input WattHours
--   , minVoltage :: Field s Text Input Volts
--   , maxVoltage :: Field s Text Input Volts
--   , batteryType :: Field s Text (Dropdown 'One) BatteryType
--   } deriving (Generic)

-- UpdateInstances(StorageUpdate)

-- instance ( NFData (Field s Text Input (WattHours))
--          , NFData (Field s Text Input (Volts))
--          , NFData (Field s Text Input (Watts))
--          , NFData (Field s Text (Dropdown 'One) (BatteryType))
--          ) => NFData (StorageUpdate s)

-- instance Validate StorageUpdate where
--   rules = StorageUpdate
--     { capacity = positive
--     , minVoltage = positive
--     , maxVoltage = positive
--     , batteryType = maybe (throwError "Cannot be empty") pure
--     }

-- storageForm :: StorageUpdate 'Edit
-- storageForm = StorageUpdate
--     { capacity = Input Clean 0
--     , minVoltage = Input Clean 0
--     , maxVoltage = Input Clean 0
--     , batteryType = fullOptions
--     }

-- addBattery :: (MonadJSM m) => StorageUpdate 'Edit -> Html m (StorageUpdate 'Edit)
-- addBattery bc = H.div [ ]
--       [ realControl @WattHours #capacity "Battery Capacity" errs bc
--       , realControl @Volts #minVoltage "Minimum Battery Voltage" errs bc
--       , realControl @Volts #maxVoltage "Maximum Battery Voltage" errs bc
--       , selectControl @'One @BatteryType #batteryType "Battery Type" errs bc
--       ]
--   where
--     errs = validate bc


-- data GenerationUpdate (s :: Status) = GenerationUpdate
--   { genPower :: Field s Text Input Watts
--   , openCircuitVoltage :: Field s Text Input Volts
--   , voltageAtMPP :: Field s Text Input Volts
--   , currentAtMPP :: Field s Text Input Amperes 
--   } deriving (Generic)

-- instance ( NFData (Field s Text Input (Amperes))
--          , NFData (Field s Text Input (Volts))
--          , NFData (Field s Text Input (Watts))
--          ) => NFData (GenerationUpdate s)

-- UpdateInstances(GenerationUpdate)

-- instance Validate GenerationUpdate where
--   rules = GenerationUpdate
--     { genPower = positive
--     , openCircuitVoltage = positive
--     , voltageAtMPP = positive
--     , currentAtMPP = positive
--     }

-- generationForm :: GenerationUpdate 'Edit
-- generationForm = GenerationUpdate
--   { genPower = Input Clean 0
--   , openCircuitVoltage = Input Clean 0
--   , voltageAtMPP = Input Clean 0
--   , currentAtMPP = Input Clean 0 
--   }


-- addGeneration :: (MonadJSM m) => GenerationUpdate 'Edit -> Html m (GenerationUpdate 'Edit)
-- addGeneration ef = H.div genProps [
--   realControl @Watts #genPower "Panel Power" errs ef
--   , realControl @Volts #openCircuitVoltage "Open Circuit Voltage" errs ef
--   , realControl @Volts #voltageAtMPP "Voltage @ Max Power Point" errs ef
--   , realControl @Amperes #currentAtMPP "Current @ Max Power Point" errs ef
--   ]
--   where
--     genProps = []
--     errs = validate ef

-- data LoadUpdate (s :: Status) = LoadUpdate
--   { loadPowerU :: Field s Text Input Watts
--   , loadDuration :: Field s Text Input Hours
--   } deriving (Generic)


-- instance ( NFData (Field s Text Input (Watts))
--          , NFData (Field s Text Input (Hours))
--          ) => NFData (LoadUpdate s)

-- UpdateInstances(LoadUpdate)

-- instance Validate LoadUpdate where
--   rules = LoadUpdate { loadPowerU = positive
--                      , loadDuration = positive
--                      }

-- loadForm :: LoadUpdate 'Edit
-- loadForm = LoadUpdate
--   { loadPowerU = Input Clean 0
--   , loadDuration = Input Clean 0
--   }

-- addLoad :: (MonadJSM m) => LoadUpdate 'Edit -> Html m (LoadUpdate 'Edit) 
-- addLoad ef = H.div loadProps 
--   [ realControl @Watts (#loadPowerU) "Load Power" errs ef
--   , realControl @Hours (#loadDuration) "Load Duration" errs ef
--   ]
--   where
--     loadProps = []
--     errs = validate ef


-- data HWUpdate (s :: Status) = HWUpdate
--   { storageU :: StorageUpdate s
--   , generationU :: GenerationUpdate s
--   , loadU :: LoadUpdate s
--   } deriving (Generic)




-- type NFDataHW s = (NFData (Field s Text Input (WattHours))
--          , NFData (Field s Text Input (Amperes))
--          , NFData (Field s Text Input (Volts))
--          , NFData (Field s Text Input (Watts))
--          , NFData (Field s Text Input (Hours))
--          , NFData (Field s Text (Dropdown 'One) (BatteryType)))

-- instance ( NFDataHW s
--          ) => NFData (HWUpdate s)

-- UpdateInstances(HWUpdate)

-- instance Validate HWUpdate where
--   rules = HWUpdate { storageU = rules, generationU = rules, loadU = rules }
--   validate (HWUpdate{storageU, generationU, loadU}) = HWUpdate
--     { storageU = validate storageU
--     , generationU = validate generationU
--     , loadU = validate loadU
--     }
--   getValid (HWUpdate{storageU,generationU, loadU}) = case getValid storageU of
--     Nothing -> Nothing
--     Just x -> case getValid generationU of
--       Nothing -> Nothing
--       Just y -> case getValid loadU of
--         Nothing -> Nothing
--         Just z -> Just (HWUpdate { storageU = x
--                                  , generationU = y
--                                  , loadU = z})

-- emptyHWForm :: HWUpdate 'Edit
-- emptyHWForm = HWUpdate
--   { storageU = storageForm
--   , generationU = generationForm
--   , loadU = loadForm
--   }

