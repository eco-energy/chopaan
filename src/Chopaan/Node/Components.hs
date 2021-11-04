{-# LANGUAGE GADTs, TypeOperators, DeriveGeneric, DeriveAnyClass, DeriveFunctor, DeriveFoldable, OverloadedStrings, DerivingVia, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, FlexibleContexts, DeriveTraversable, TypeApplications, CPP #-}

module Chopaan.Node.Components where

import GHC.Generics
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON(..), FromJSON(..), genericParseJSON, genericToEncoding, genericToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T

import Shpadoinkle.Widgets.Types (Humanize(..), Present)

import Data.Greskell.GraphSON.GValue (unwrapOne, unwrapAll)

import Data.Greskell (FromGraphSON(..), Key(..), PMap
                     , GValue, Single, Parser
                     , parseUnwrapList, parseJSONViaGValue, (.:)
                     , lookup, lookupM)
import Chopaan.Graph.Greskell

#ifndef ghcjs_HOST_OS
import NetSpider.Graph (NodeAttributes(..), VFoundNode)
#endif

import Data.Greskell.Extra (writeKeyValues, (<=:>), pMapToFail, lookupAs)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (fromStrict)
import Data.Binary


data BatteryType = LeadAcidFlooded | LeadAcidSealed | LithiumIon
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Humanize, Present,
            Generic, Binary, ToJSON, FromJSON, NFData)


instance Humanize (Maybe BatteryType) where
  humanize = maybe "select battery" humanize

instance Semigroup BatteryType where (<>) = min
instance Monoid BatteryType where mempty = minBound

instance FromGraphSON BatteryType where
  parseGraphSON = parseJSON . unwrapOne

data BatteryConf a = BatteryConf
  { minV :: a
  , maxV :: a
  , capacityAH :: a
  , batType :: BatteryType
  } deriving (Eq, Ord, Show, Read, Generic, Binary, NFData, ToJSON, FromJSON, Functor, Foldable, Traversable)

instance (GreskellC a, Num a) => FromGraphSON (BatteryConf a) where
  parseGraphSON = parseUnwrapTraversable

#ifndef ghcjs_HOST_OS
minVKey :: (GreskellC a, Num a) => Key VFoundNode a
minVKey = "minV"
maxVKey :: (GreskellC a, Num a) => Key VFoundNode a
maxVKey = "maxV"
capacityAHKey :: (GreskellC a, Num a) => Key VFoundNode a
capacityAHKey = "capacityAH"
batTypeKey :: (GreskellC a) => Key VFoundNode a
batTypeKey = "batType"

instance (GreskellC a, Num a) => NodeAttributes (BatteryConf a) where
  writeNodeAttributes bc = fmap writeKeyValues $ sequence $
    [ minVKey <=:> minV bc
    , maxVKey <=:> maxV bc
    , capacityAHKey <=:> capacityAH bc
    , batTypeKey <=:> batType bc
    ]
  parseNodeAttributes props = pMapToFail (BatteryConf
                                          <$> lookupAs minVKey props
                                          <*> lookupAs maxVKey props
                                          <*> lookupAs capacityAHKey props
                                          <*> lookupAs batTypeKey props
                                         )
#endif

defBC :: Num a => BatteryConf a
defBC = BatteryConf 0 0 0 LeadAcidFlooded

data BatteryTop a = ParBC (BatteryConf a) (BatteryConf a)
                  | SeqBC (BatteryConf a) (BatteryConf a)
                  | SingBC (BatteryConf a)
                  deriving (Eq, Ord, Show, Read, Generic, Binary, NFData, Functor, Foldable, Traversable)

batEncodingOpts = optSumEncoding "batteryTag" "batteryContent"
pvEncodingOpts = optSumEncoding "pvTag" "pvContent"
ldEncodingOpts = optSumEncoding "loadTag" "loadContent"


instance (Binary a) => ToJSON (BatteryTop a) where
  toJSON = wineryJSONWrite --genericToJSON pvEncodingOpts
  toEncoding = wineryJSONEncode

instance (Binary a) => FromJSON (BatteryTop a) where
  parseJSON = wineryJSONRead "BatteryTop"


instance (GreskellC a, Num a, Read a) => FromGraphSON (BatteryTop a) where
  parseGraphSON = parseJSON . unwrapAll

type VI a = (a, a)

type EvolveB a = (BatteryConf a -> VI a -> VI a)

data Battery a where
  ParB :: Battery a -> Battery a -> Battery a
  SeqB :: Battery a -> Battery a -> Battery a
  ABattery :: BatteryConf a -> EvolveB a -> Battery a
  deriving (Generic, NFData)


runBB :: (Fractional a) => Battery a -> VI a -> VI a
runBB (ParB a b) (v , i) = combinePar (runBB a (v , (i/2))) (runBB b (v , (i/2)))
runBB (SeqB a b) (v , i) = combineSeq (runBB a (v/2 , i)) (runBB b (v/2 , i))
runBB (ABattery bConf evolve) vi = evolve bConf vi 

combinePar :: VI a -> VI a -> VI a
combinePar = undefined

combineSeq :: VI a -> VI a -> VI a
combineSeq = undefined


data PVConf a = PVConf
  { openCircuitV :: a
  , vAtMPP :: a
  , iAtMPP :: a
  , pvPower :: a
  } deriving (Eq, Ord, Show, Read, Generic, Binary, NFData, ToJSON, FromJSON, Functor, Foldable, Traversable)

instance (GreskellC a, Num a) => FromGraphSON (PVConf a) where
  parseGraphSON = parseUnwrapTraversable


defPC :: Num a => PVConf a
defPC = PVConf 0 0 0 0

data PVTop a = ParPC (PVConf a) (PVConf a)
             | SeqPC (PVConf a) (PVConf a)
             | SingPC (PVConf a)
             deriving (Eq, Ord, Show, Read, Generic, Binary, NFData, Functor, Foldable, Traversable)


instance (Binary a) => ToJSON (PVTop a) where
  toJSON = wineryJSONWrite
  toEncoding = wineryJSONEncode

instance (Binary a) => FromJSON (PVTop a) where
  parseJSON = wineryJSONRead "PVTop"

instance (GreskellC a, Num a) => FromGraphSON (PVTop a) where
  parseGraphSON = parseJSON . unwrapAll
    

data PVEnv a = PVEnv deriving (Eq, Ord, Show, Read, Generic, Binary, NFData)

type EvolvePV a = (PVConf a -> PVEnv a -> VI a)

data PV a where
  ParPV :: PV a -> PV a -> PV a
  SeqPV :: PV a -> PV a -> PV a
  APV :: PVConf a -> EvolvePV a -> PV a
  deriving (Generic, NFData)

-- $ PVEnv is time, location and temperature.
-- $ The evolve function must determine what the output voltage and current is
runPV :: (Fractional a) => PV a -> PVEnv a -> VI a
runPV (ParPV a b) p = combinePar (runPV a p) (runPV b p)
runPV (SeqPV a b) p = combineSeq (runPV a p) (runPV b p)
runPV (APV bConf evolve) p = evolve bConf p


data LoadConf a = LoadConf
  { loadPower :: a
  , loadName :: T.Text
  } deriving (Eq, Ord, Show, Read, Generic, Binary, NFData, ToJSON, FromJSON, Functor, Foldable, Traversable)

instance (GreskellC a, Num a) => FromGraphSON (LoadConf a) where
  parseGraphSON = parseUnwrapTraversable
  

defLC :: Num a => LoadConf a
defLC = LoadConf 0 "No_LC"

data Load a where
  ParLoad :: Load a -> Load a -> Load a
  ALoad :: LoadConf a -> Load a
  deriving (Generic, Binary, NFData, ToJSON, FromJSON)


data LoadTop a = ParLC (LoadConf a) (LoadConf a)
               | SingLC (LoadConf a)
  deriving (Eq, Ord, Show, Read, Generic, Binary, NFData, Functor, Foldable, Traversable)



instance (Binary a, Show a) => ToJSON (LoadTop a) where
  toJSON = wineryJSONWrite --genericToJSON pvEncodingOpts
  toEncoding = wineryJSONEncode

instance (Binary a, Show a) => FromJSON (LoadTop a) where
  parseJSON = wineryJSONRead "LoadTop"

instance (GreskellC a, Num a) => FromGraphSON (LoadTop a) where
  parseGraphSON = parseJSON . unwrapAll
