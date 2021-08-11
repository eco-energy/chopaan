{-# LANGUAGE RecordWildCards, NamedFieldPuns, TypeApplications, DeriveFunctor, OverloadedStrings, FlexibleContexts, ConstraintKinds, NoMonomorphismRestriction, ScopedTypeVariables, PackageImports #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveTraversable, DerivingStrategies, DerivingVia, StandaloneDeriving, PackageImports, CPP, ExtendedDefaultRules #-}
module Chopaan.Node.Metrics where

import GHC.Generics hiding (R)
import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Lens
import Data.Binary (Binary)
import qualified Data.Binary as B
import qualified "base64" Data.ByteString.Base64 as B64
import Data.Time
import Data.Aeson hiding (encode, decode)
import qualified Data.Aeson as A
import Data.Either
import Data.Bifunctor

import qualified Data.HashMap.Strict as HM
import qualified Data.Csv as Csv hiding (encode)
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

import Shpadoinkle.Widgets.Types (Humanize(..))

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
import Chopaan.Node.Storage
import Chopaan.Utils.JSON
import Chopaan.Utils.Time
import Chopaan.Graph.Greskell


default(T.Text)


toField = Csv.toField
toNamedRecord = Csv.toNamedRecord
headerOrder = Csv.headerOrder

{----- Basic Types ------}



newtype WattSeconds = WS { unWs :: Compensated Double }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Num, Fractional, Binary, Real, RealFrac, NFData)
  deriving anyclass (Humanize)

newtype Watts = W { unW :: Compensated Double }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Num, Fractional, Real, Binary, RealFrac, NFData)
  deriving anyclass (Humanize)

instance Show WattSeconds where
  show = (printf ("%.2g")) . fromWattSeconds

instance Show Watts where
  show = (printf ("%.2g")) . fromWatts


fromWatts :: Watts -> Double
fromWatts = uncompensated . unW

fromWattSeconds :: WattSeconds -> Double
fromWattSeconds = uncompensated . unWs

toWatts :: Double -> Watts
toWatts a = W $ add a 0 compensated

toWattSeconds :: Double -> WattSeconds
toWattSeconds a = WS $ add a 0 compensated

pToE :: (Real t) => t -> Watts -> WattSeconds
pToE t (W p') = WS $ (*^) (realToFrac t) p'

instance Csv.ToField (Watts) where
  toField = toField . uncompensated . unW

instance Csv.ToField (WattSeconds) where
  toField = toField . uncompensated . unWs

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
  { tx :: ! a
  , consumed :: !a
  , generated :: !a
  } deriving (Eq, Ord, Show, Binary, Generic, Functor, NFData, ToJSON, FromJSON, Humanize)
  

instance (Csv.ToField a) => Csv.ToNamedRecord (Node a)

instance Csv.DefaultOrdered (Node a) where
  headerOrder _ = Vec.fromList ["tx", "consumed", "generated"]

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
nodeKey :: Key n BL.ByteString
nodeKey = "nodeKey"

               
instance (GreskellC a) => LinkAttributes (Node a) where
  writeLinkAttributes node = fmap writeKeyValues $ sequence $
    [ nodeKey <=:> A.encode node ]
  parseLinkAttributes props = pMapToFail $ decodeBin $ lookupAs nodeKey props
               
instance (GreskellC a) => NodeAttributes (Node a) where
  writeNodeAttributes node = fmap writeKeyValues $ sequence $
    [ nodeKey <=:> A.encode node ]
  parseNodeAttributes props = pMapToFail $ decodeBin $ lookupAs nodeKey props

instance (GreskellC a) => FromGraphSON (Node a) where
  parseGraphSON = parseJSON . unwrapAll
#endif



instance Binary UTCTime
instance Binary DiffTime where
  put a = B.put @Int $ round a
  get = secondsToDiffTime <$> B.get 


data SensorMetrics e p = SensorMetrics
  { _time :: !(Maybe UTCTime)
  , lastTimeDiff :: !DiffTime
  , _powerT :: !(Node p)
  , _energyT :: !(Node e)
  , _battery :: !(Battery e p)
  , _demand :: !e
  , _sensors :: EnergyState
  } deriving (Eq, Ord, Generic, Show, NFData, ToJSON, FromJSON, Humanize)

initSM :: (Fractional e, Fractional p) => SensorMetrics e p
initSM = SensorMetrics Nothing 0 mempty mempty emptyB 0 zeroMsg 
  

-- instance (Binary e, Binary p) => ToJSON (SensorMetrics e p) where
--     toJSON = binaryJSONWrite --genericToJSON pvEncodingOpts
--     toEncoding = binaryJSONEncode


-- instance (Binary e, Binary p) => FromJSON (SensorMetrics e p) where
--   parseJSON = binaryJSONRead "SensorMetrics"

#ifndef ghcjs_HOST_OS
timeKey :: Key VFoundNode (Maybe UTCTime)
timeKey = "timeKey"

timeDiffKey :: Key VFoundNode (DiffTime)
timeDiffKey = "timeDiffKey"

powerKey :: Key VFoundNode (BL.ByteString)
powerKey = "powerKey"

energyKey :: Key VFoundNode (BL.ByteString)
energyKey = "energyKey"

batteryKey :: Key VFoundNode (BL.ByteString)
batteryKey = "batteryKey"

demandKey :: Key VFoundNode (BL.ByteString)
demandKey = "demandKey"

esMsgKey :: Key VFoundNode (BL.ByteString)
esMsgKey = "esMsg"

instance FromJSON B.ByteString where
  parseJSON (String t) = pure $ (either (const "") id . B64.decodeBase64 . T.encodeUtf8) t
  parseJSON _ = empty

instance ToJSON B.ByteString where
  toJSON = String . T.decodeUtf8 . B64.encodeBase64'

instance FromJSON BL.ByteString where
  parseJSON a = (pure . BL.fromStrict) =<< A.parseJSON a

instance ToJSON BL.ByteString where
  toJSON = String . T.decodeUtf8 . B64.encodeBase64' . BL.toStrict

instance FromGraphSON BL.ByteString where
  parseGraphSON = parseJSON . unwrapOne

instance (GreskellC e, GreskellC p) => NodeAttributes (SensorMetrics e p) where
  writeNodeAttributes SensorMetrics{..} = fmap writeKeyValues $ sequence $
    [ timeKey <=?> _time
    , timeDiffKey <=:> lastTimeDiff
    , powerKey <=:> A.encode _powerT
    , energyKey <=:> A.encode _energyT
    , batteryKey <=:> A.encode _battery
    , demandKey <=:> A.encode _demand
    , esMsgKey <=:> A.encode _sensors
    ]
  parseNodeAttributes props = pMapToFail (SensorMetrics
                                          <$> lookupAs' timeKey props
                                          <*> lookupAs timeDiffKey props
                                          <*> (decodeBin $ lookupAs powerKey props)
                                          <*> (decodeBin $ lookupAs energyKey props)
                                          <*> (decodeBin $ lookupAs batteryKey props)
                                          <*> (decodeBin $ lookupAs demandKey props)
                                          <*> (decodeBin $ lookupAs esMsgKey props)
                                         )
decodeBin :: (FromJSON a) => Either PMapLookupException BL.ByteString -> Either PMapLookupException a 
decodeBin (Left a) = (Left a)
decodeBin (Right x) = case A.decode x of
  Nothing -> (Left $ PMapParseError "sensorMetric Key" "aeson decode failed for sensor metrics")
  Just x' -> Right x'


#endif
--instance Binary EnergyState where
--  encode = undefined

instance ToJSON (StreamState) where
  toJSON a = toJSON . fromEnum $ a

instance FromJSON (StreamState) where
  parseJSON a = toEnum <$> (parseJSON a)

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
    , "cpu_time" A..= (a ^. cpuTime)
    , "status" A..= (a ^. status)
    , "gridCurrent" A..= (a ^. gridCurrent)
    , "solarVoltage" A..= (a ^. solarVoltage)
    ]
  toEncoding = messageToEncoding

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
    x9 <- (v .: "cpu_time")
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
  --parseJSON _ = error "MUST BE OBJECT"
  

esFieldNamesJSON :: [T.Text]
esFieldNamesJSON = ["batteryVoltage",
                     "gridVoltage",
                     "batteryToLoadCurrent",
                     "batteryToGridCurrent",
                     "gridToBatteryCurrent",
                     "solarInputCurrent",
                     "temperature",
                     "dutyCycle",
                     "cpu_time",
                     "status",
                     "gridCurrent",
                     "solarVoltage" 
                   ]

fieldAccessorsJSON es = es ^.. ( batteryVoltage
                          <> gridVoltage
                          <> batteryToLoadCurrent
                          <> batteryToGridCurrent
                          <> gridToBatteryCurrent
                          <> solarInputCurrent
                          <> temperature
                          <> dutyCycle
                          <> cpuTime
                          <> status
                          <> gridCurrent
                          <> solarVoltage
                        )
#ifndef ghcjs_HOST_OS
esFieldNamesCSV :: [Csv.Name]
esFieldNamesCSV = ["batteryV",
                   "gridV",
                   "battery2LoadC",
                   "battery2GridC",
                   "grid2BatteryC",
                   "solarC",
                   "dutyC"
                  ]
fieldAccessorsCSV es = es ^.. ( batteryVoltage
                          <> gridVoltage
                          <> batteryToLoadCurrent
                          <> batteryToGridCurrent
                          <> gridToBatteryCurrent
                          <> solarInputCurrent
                          <> dutyCycle
                        )

instance Csv.ToNamedRecord EnergyState where
  toNamedRecord es = HM.fromList $
                zip esFieldNamesCSV $
                map (pack . show) $
                fieldAccessorsCSV es


instance Csv.DefaultOrdered EnergyState where
  headerOrder _ = Vec.fromList esFieldNamesCSV

instance Csv.ToField UTCTime where
  toField t = pack (show t)

instance (Csv.ToField e, Csv.ToField p) => Csv.ToNamedRecord (SensorMetrics e p) where
  toNamedRecord (SensorMetrics {..}) = foldl (HM.union) (HM.fromList [("time", toField _time)])
    [ toNamedRecord _battery,
      toNamedRecord _powerT,
      toNamedRecord _energyT,
      --toNamedRecord _sensorsT,
      HM.fromList [("demand", toField _demand)]
    ]
#endif


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
    -- <> sep <> ("sensor readings:" <> sep <> (show (pprintMessage _sensorsT)))
    where
      sep = "\n"

secsToMinutes :: (Num a) => a -> a
secsToMinutes = (* 60)

nmFilter :: (NodeId a) -> SensorMetrics e p -> Bool
nmFilter _ = isJust . _time

#ifndef ghcjs_HOST_OS
instance Csv.DefaultOrdered (SensorMetrics e p)
#endif

type Timestamp = (Maybe UTCTime, DiffTime)

type BatteryR = Battery WattSeconds Watts

data Battery e p = Battery
  { soc :: !e
  , chargeLim :: !p
  , dischargeLim :: !p
  , totalCapacity :: !e
  } deriving (Eq, Ord, Show, Binary, Generic, NFData, Functor, Humanize)

instance Bifunctor Battery where
  bimap f g Battery{soc, chargeLim, dischargeLim, totalCapacity} = Battery
    { soc = f soc
    , chargeLim = g chargeLim
    , dischargeLim = g dischargeLim
    , totalCapacity = f totalCapacity
    } 


instance (ToJSON e, ToJSON p) => ToJSON (Battery e p)
instance (FromJSON e, FromJSON p) => FromJSON (Battery e p)

#ifndef ghcjs_HOST_OS
instance (GreskellC e, GreskellC p) => FromGraphSON (Battery e p) where
  parseGraphSON = parseJSON . unwrapAll

batKey :: Key VFoundNode BL.ByteString
batKey = "battKey"

instance (GreskellC e, GreskellC p) => NodeAttributes (Battery e p) where
  writeNodeAttributes bat = fmap writeKeyValues $ sequence $
    [ batKey <=:> A.encode bat ]
  parseNodeAttributes props = pMapToFail $ decodeBin $ lookupAs batKey props
#endif

emptyB :: (Fractional e, Fractional p) => Battery e p
emptyB = Battery 0 0 0 0

#ifndef ghcjs_HOST_OS
instance Csv.DefaultOrdered (Battery e p)
instance (Csv.ToField e, Csv.ToField p) => Csv.ToNamedRecord (Battery e p)
#endif

instance (Fractional e, Fractional p, Ord e, Ord p) => Semigroup (Battery e p) where
  b <> b' = (emptyB @e @p) { soc = min (soc b)  (soc b')
                           , chargeLim = min (chargeLim b) (chargeLim b')
                           , dischargeLim = min (dischargeLim b) (dischargeLim b')
                           , totalCapacity = min (totalCapacity b) (totalCapacity b')
                           }

instance (Fractional e, Fractional p, Ord e, Ord p) => Monoid (Battery e p) where
  mempty = emptyB

runTime :: ParamType a => Battery a p -> SensorVector a -> a
runTime Battery{soc} SensorVector{..} = soc / ((normC sensorCurrent) * sensorTerminalV)
  where
    normC c
      | c >= 0 = c
      | c < 0 = 0.05
      | otherwise = error "neither greater nor less than nor equal to zero"
      

socPercentage :: Fractional e => Battery e p -> e
socPercentage Battery{..} = (soc * 100 / totalCapacity)




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
    i = - (es ^. gridToBatteryCurrent + es ^. solarInputCurrent)
    o = es ^. batteryToGridCurrent + es ^. batteryToLoadCurrent
{-# INLINE storageSensors #-}

power :: EnergyState -> PowerNR
power es = Node
           { tx = txIn' - txOut'
           , consumed = cnsm'
           , generated = genP' }
  where
    txIn' = p batteryVoltage gridToBatteryCurrent
    txOut' = p batteryVoltage batteryToGridCurrent
    cnsm' = p batteryVoltage batteryToLoadCurrent
    genP' = p batteryVoltage solarInputCurrent
    p v i = toWatts $ (es ^. i) * v'
      where
        v' = (es ^. v)
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


{----------------------------------------------------------

                CSV Conversion
----------------------------------------------------------}


#ifndef ghcjs_HOST_OS
-- Identified sensor type for monitoring
newtype TaggedNode n e p = TaggedNode (n, SensorMetrics e p) deriving (Generic)

instance (Csv.ToField n, Csv.ToField e, Csv.ToField p) => Csv.ToNamedRecord (TaggedNode n e p) where
  toNamedRecord (TaggedNode (n, ns)) = (HM.fromList [("NodeId", toField n)]) <> toNamedRecord ns

instance Csv.DefaultOrdered (TaggedNode n e p) where
  headerOrder _ = (Vec.fromList $ ["NodeId", "time"])
                  <> (headerOrder (undefined :: EnergyState))
                  <> (headerOrder (undefined :: PowerNR))
                  <> (headerOrder (undefined :: EnergyNR))
#endif
