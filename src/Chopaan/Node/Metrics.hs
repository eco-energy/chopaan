{-# LANGUAGE RecordWildCards, NamedFieldPuns, TypeApplications, DeriveFunctor, OverloadedStrings, FlexibleContexts, ConstraintKinds, NoMonomorphismRestriction, ScopedTypeVariables, PackageImports, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveTraversable, DerivingStrategies, DerivingVia, StandaloneDeriving, PackageImports, CPP, ExtendedDefaultRules, BangPatterns, StrictData #-}
module Chopaan.Node.Metrics where

import GHC.Generics hiding (R)
import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Lens

import Data.Typeable
import Data.Selectors

import qualified Codec.Winery as W
import qualified "base64" Data.ByteString.Base64 as B64
import Data.Time
import Data.Aeson hiding (encode, decode)
import qualified Data.Aeson as A
import Data.Either
import Data.Bifunctor

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as Vec (fromList)

import Data.ByteString.Char8 (pack)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Numeric.Compensated

import Data.ProtoLens
--import Lens.Micro
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Printf


import Proto.NodeMessageSchema.NodeMessages
import Proto.NodeMessageSchema.NodeMessages_Fields

-- import Shpadoinkle.Widgets.Types (Humanize(..))

#ifndef ghcjs_HOST_OS
import ConCat.Misc (R)
#endif

{---- NetSpider Imports ----}
import Data.Greskell (Key, lookupAs, lookupAs', pMapToFail
                     , FromGraphSON(..), parseGraphSON, PMapLookupException(..))
import Data.Greskell.Extra (writeKeyValues, (<=:>), (<=?>))
import Data.Greskell.GraphSON.GValue (unwrapOne, unwrapAll)

#ifndef ghcjs_HOST_OS
import NetSpider.Graph (NodeAttributes(..), VFoundNode, LinkAttributes(..), EFinds)
import NetSpider.Timestamp (fromS)
import NetSpider.Snapshot (nodeId, nodeTimestamp)
#endif

import Chopaan.Node.NodeId
import Chopaan.Node.NodeSensors
import Chopaan.Node.Storage
import Chopaan.Node.Storage.Battery
import Chopaan.Utils.JSON
import Chopaan.Utils.Time
import Chopaan.Graph.Greskell


default(T.Text)


{----- Basic Types ------}


--deriving via (W.WineryRecord (Compensated Double)) instance W.Serialise (Compensated Double) 

deriving via (W.WineryRecord (WattSeconds)) instance W.Serialise (Compensated Double)

newtype WattSeconds = WS { unWs :: Compensated Double }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving newtype (Num, Fractional, Real, RealFrac, NFData)
  deriving (W.Serialise) via (W.WineryRecord (WattSeconds))

instance Selectors WattSeconds where
  selectors = selectorsRep @(WattSeconds)


newtype Watts = W { unW :: Compensated Double }
  deriving stock (Eq, Ord, Generic, Typeable)
  deriving newtype (Num, Fractional, Real, RealFrac, NFData)
  deriving (W.Serialise) via (W.WineryRecord Watts)

instance Selectors Watts where
  selectors = selectorsRep @(Watts)


instance Show WattSeconds where
  show = (printf ("%.2g")) . fromWattSeconds

instance Show Watts where
  show = (printf ("%.2g")) . fromWatts


fromWatts :: Watts -> Double
fromWatts = uncompensated . unW

fromWattSeconds :: WattSeconds -> Double
fromWattSeconds = uncompensated . unWs

toWatts :: Double -> Watts
toWatts !a = W $ add a 0 compensated

toWattSeconds :: Double -> WattSeconds
toWattSeconds !a = WS $ add a 0 compensated

pToE :: (Real t) => t -> Watts -> WattSeconds
pToE !t (W !p') = WS $ (*^) (realToFrac t) p'


instance ToJSON WattSeconds where
  toJSON = toJSON . uncompensated . unWs

instance ToJSON Watts where
  toJSON = toJSON . uncompensated . unW

instance FromJSON WattSeconds where
  parseJSON x = toWattSeconds <$> (A.parseJSON x)

instance FromJSON Watts where
  parseJSON x = toWatts <$> (A.parseJSON x)

instance FromGraphSON WattSeconds where
  parseGraphSON = (fmap toWattSeconds) . parseGraphSON

instance FromGraphSON Watts where
  parseGraphSON = (fmap toWatts) . parseGraphSON

-- Episodic Metrics

data Node a = Node
  { tx :: !a
  , consumed :: !a
  , generated :: !a
  }
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData, ToJSON, FromJSON)
  deriving W.Serialise via (W.WineryRecord (Node a))
  

instance (Typeable a) => Selectors (Node a) where
  selectors = selectorsRep @(Node a)


instance Applicative Node where
  pure v = Node
    { tx = v
    , consumed = v
    , generated = v
    }
  f <*> v = Node
              { tx = tx f $ tx v
              , consumed = consumed f $ consumed v
              , generated = generated f $ generated v
              }

initEA :: (Num a) => Node a
initEA = Node 0 0 0

-- Check associativity
instance (Num a) => Semigroup (Node a) where
  v1 <> v2 = Node
    { tx = tx v1 + tx v2
    , consumed = consumed v1 + consumed v2
    , generated = generated v1 + generated v2
    }

instance (Num a) => Monoid (Node a) where
  mempty = initEA


#ifndef ghcjs_HOST_OS
nodeKey :: Key n a
nodeKey = "nodeKey"

               
instance (GreskellC a) => LinkAttributes (Node a) where
  writeLinkAttributes node = fmap writeKeyValues $ sequence $
    [ nodeKey <=:> wineryJSONWrite node ]
  parseLinkAttributes props = decodeBin "Node As Link" $ lookupAs nodeKey props
               
instance (GreskellC a) => NodeAttributes (Node a) where
  writeNodeAttributes node = fmap writeKeyValues $ sequence $
    [ nodeKey <=:> wineryJSONWrite node ]
  parseNodeAttributes props = decodeBin "Node as Node" $ lookupAs nodeKey props

instance (GreskellC a) => FromGraphSON (Node a) where
  parseGraphSON = parseJSON . unwrapAll
#endif




data SensorMetrics e p = SensorMetrics
  { _time :: !(Maybe UTCTime)
  , lastTimeDiff :: !NominalDiffTime
  , _powerT :: !(Node p)
  , _energyT :: !(Node e)
  , _battery :: !(Battery e p)
  , _demand :: !e
  , _sensors :: !(NodeT' Double)
  }
  deriving (Eq, Ord, Generic, Show, NFData, ToJSON, FromJSON)
  deriving (W.Serialise) via (W.WineryRecord (SensorMetrics e p))

instance (Typeable e, Typeable p) => Selectors (SensorMetrics e p) where
  selectors = selectorsRep @(SensorMetrics e p)

initSM :: (Fractional e, Fractional p) => SensorMetrics e p
initSM = SensorMetrics Nothing 0 mempty mempty emptyB 0 (fromNodeMessage zeroMsg) 
  

-- instance (Binary e, Binary p) => ToJSON (SensorMetrics e p) where
--     toJSON = wineryJSONWrite --genericToJSON pvEncodingOpts
--     toEncoding = wineryJSONEncode


-- instance (Binary e, Binary p) => FromJSON (SensorMetrics e p) where
--   parseJSON = wineryJSONRead "SensorMetrics"

#ifndef ghcjs_HOST_OS
timeKey :: Key VFoundNode (Maybe UTCTime)
timeKey = "timeKey"

timeDiffKey :: Key VFoundNode (NominalDiffTime)
timeDiffKey = "timeDiffKey"

powerKey :: Key VFoundNode (a)
powerKey = "powerKey"

energyKey :: Key VFoundNode (a)
energyKey = "energyKey"

batteryKey :: Key VFoundNode (a)
batteryKey = "batteryKey"

demandKey :: Key VFoundNode (a)
demandKey = "demandKey"

esMsgKey :: Key VFoundNode (a)
esMsgKey = "esMsg"


instance (GreskellC e, GreskellC p) => NodeAttributes (SensorMetrics e p) where
  writeNodeAttributes SensorMetrics{..} = fmap writeKeyValues $ sequence $
    [ timeKey <=?> _time
    , timeDiffKey <=:> lastTimeDiff
    , powerKey <=:> wineryJSONWrite _powerT
    , energyKey <=:> wineryJSONWrite _energyT
    , batteryKey <=:> wineryJSONWrite _battery
    , demandKey <=:> wineryJSONWrite _demand
    , esMsgKey <=:> wineryJSONWrite _sensors
    ]
  parseNodeAttributes props = (SensorMetrics
                                <$> (pMapToFail $ lookupAs' timeKey props)
                                <*> (pMapToFail $ lookupAs timeDiffKey props)
                                <*> (decodeBin "sensorM: power" $ lookupAs powerKey props)
                                <*> (decodeBin "sensorM: energy" $ lookupAs energyKey props)
                                <*> (decodeBin "sensorM: battery" $ lookupAs batteryKey props)
                                <*> (decodeBin "sensorM: demand" $ lookupAs demandKey props)
                                <*> (decodeBin "sensorM: message" $ lookupAs esMsgKey props)
                              )
#endif
--instance Binary EnergyState where
--  encode = undefined

instance ToJSON (StreamState) where
  toJSON a = toJSON . fromEnum $ a

instance FromJSON (StreamState) where
  parseJSON a = toEnum <$> (parseJSON a)

--instance Selectors (EnergyState) where
--  selectors = selectorsRep @(EnergyState)

instance ToJSON (EnergyState) where
  toJSON a = object $ [
    "batteryVoltage" A..= (a ^. batteryVoltage)
    , "gridVoltage" A..= (a ^. gridVoltage)
    , "batteryToLoadCurrent" A..= (a ^. batteryToLoadCurrent)
    , "batteryToGridCurrent" A..= (a ^. batteryToGridCurrent)
    , "gridToBatteryCurrent" A..= (a ^. gridToBatteryCurrent)
    , "solarInputCurrent" A..= (a ^. solarInputCurrent)
    , "temperature" A..= (a ^. temperature)
    , "dutyCycle" A..= (a ^. dutyCycle)
    , "cpuTime" A..= (a ^. cpuTime)
    , "status" A..= (a ^. status)
    , "gridCurrent" A..= (a ^. gridCurrent)
    , "solarVoltage" A..= (a ^. solarVoltage)
    ]
  --toEncoding = messageToEncoding

instance FromJSON (EnergyState) where
  parseJSON = withObject "EnergyState" $ \v -> do
    let m = defMessage
    x1 <- (v .: "batteryVoltage")
    x2 <- (v .: "gridVoltage")
    x3 <- (v .: "batteryToLoadCurrent")
    x4 <- (v .: "batteryToGridCurrent")
    x5 <- (v .: "gridToBatteryCurrent")
    x6 <- (v .: "solarInputCurrent")
    x7 <- (v .: "temperature")
    x8 <- (v .: "dutyCycle")
    x9 <- (v .: "cpuTime")
    x10 <- (v .: "status")
    x11 <- (v .: "gridCurrent")
    x12 <- (v .: "solarVoltage")
    return $ m
      & batteryVoltage .~ x1
      & gridVoltage .~ x2
      & batteryToLoadCurrent .~ x3
      & batteryToGridCurrent .~ x4
      & gridToBatteryCurrent .~ x5
      & solarInputCurrent .~ x6
      & temperature .~ x7
      & dutyCycle .~ x8
      & cpuTime .~ x9
      & status .~ x10
      & gridCurrent .~ x11
      & solarVoltage .~ x12
    where
      revC mv a = do
        s <- mv
        a .~ mv
        return a


showDec :: Double -> String
showDec = (printf ("%.2g"))

prettyShow :: (Show e, Show p, Fractional e) => SensorMetrics e p -> String
prettyShow SensorMetrics{..} = ("last connection: " <> show _time)
    <> sep <> ("battery energy stored (Ws): "
    <> sep <> (show $ (socPercentage _battery) * (totalCapacity _battery)))
    -- <> sep <> ("runtime estimate :" <> sep <> showDec (secsToMinutes $ runTime @R _battery (storageSensors _sensorsT)))
    <> sep <> ("current demand (Ws): " <> show _demand)
    <> sep <> ("current power:" <> sep <> show _powerT)
    <> sep <> ("current energy:" <> sep <> show _energyT)
    <> sep <> ("sensor readings:" <> sep <> (show _sensors))
    where
      sep = "\n"

secsToMinutes :: (Num a) => a -> a
secsToMinutes = (* 60)

nmFilter :: (NodeId a) -> SensorMetrics e p -> Bool
nmFilter _ = isJust . _time


type Timestamp = (Maybe UTCTime, NominalDiffTime)

type BatteryR = Battery WattSeconds Watts






{---------------------------------------------------------------------------------------------------------------------

                                          Helper Functions
---------------------------------------------------------------------------------------------------------------------}


type PowerN p = Node p

type EnergyN e = Node e

type PowerNR = EnergyN Watts

type EnergyNR = EnergyN WattSeconds


storageSensors :: EnergyState -> SensorVector Double
storageSensors es = SensorVector
  { sensorTerminalV = es ^. batteryVoltage
  , sensorCurrent =  i + o
  }
  where
    !i = - (es ^. gridToBatteryCurrent + es ^. solarInputCurrent)
    !o = es ^. batteryToGridCurrent + es ^. batteryToLoadCurrent
{-# INLINE storageSensors #-}

power :: EnergyState -> PowerNR
power !es = Node
           { tx = txIn' - txOut'
           , consumed = cnsm'
           , generated = genP' }
  where
    !txIn' = p batteryVoltage gridToBatteryCurrent
    !txOut' = p batteryVoltage batteryToGridCurrent
    !cnsm' = p batteryVoltage batteryToLoadCurrent
    !genP' = p batteryVoltage solarInputCurrent
    p !v !i = toWatts $ (es ^. i) * v'
      where
        !v' = (es ^. v)
{-# INLINE power #-}

utcTimeES :: EnergyState -> UTCTime
utcTimeES = utcTimeNow . (^. cpuTime)
{-# INLINE utcTimeES #-}


zeroMsg :: EnergyState
zeroMsg = defMessage
               & batteryVoltage .~ 0
               & gridVoltage .~ 0
               & batteryToLoadCurrent .~ 0
               & batteryToGridCurrent .~ 0
               & gridToBatteryCurrent .~ 0
               & solarInputCurrent .~ 0
               & dutyCycle .~ 0
               & cpuTime .~ 0

