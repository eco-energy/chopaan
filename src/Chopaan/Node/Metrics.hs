{-# LANGUAGE RecordWildCards, NamedFieldPuns, TypeApplications, DeriveFunctor, OverloadedStrings, FlexibleContexts, ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DeriveAnyClass, DeriveFoldable, DeriveFunctor, DeriveTraversable, DerivingStrategies, DerivingVia, StandaloneDeriving #-}
module Chopaan.Node.Metrics where

import GHC.Generics hiding (R)
import Control.DeepSeq (NFData)

import Data.Time
import Data.Aeson hiding (encode, decode)
import qualified Data.Aeson as A


import qualified Data.HashMap.Strict as HM
import Data.Csv hiding ((.:))
import qualified Data.Vector as Vec (fromList)

import Data.ByteString.Char8 (pack)
import Data.Maybe (isJust)
import Numeric.Compensated

import Data.ProtoLens
import Lens.Micro
import qualified Data.Text as T
import Text.Printf


import Proto.NodeMessageSchema.NodeMessages hiding (NodeId)
import Proto.NodeMessageSchema.NodeMessages_Fields
import ConCat.Misc (R)

{---- NetSpider Imports ----}
import Data.Greskell (Key, lookupAs, pMapToFail, FromGraphSON(..), parseGraphSON)
import Data.Greskell.Extra (writeKeyValues, (<=:>))
import Data.Greskell.GraphSON.GValue (unwrapOne, unwrapAll)


import NetSpider.Graph (NodeAttributes(..), VFoundNode)
import NetSpider.Timestamp (fromS)
import NetSpider.Snapshot (nodeId, nodeTimestamp)

import Chopaan.Node.NodeId
import Chopaan.Node.Storage
import Chopaan.Utils.JSON
import Chopaan.Utils.Time
import Chopaan.Graph.Greskell


{----- Basic Types ------}


newtype WattSeconds = WS { unWs :: Compensated Double }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Num, Fractional, Real, RealFrac, NFData)

newtype Watts = W { unW :: Compensated Double }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Num, Fractional, Real, RealFrac, NFData)

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

instance ToField (Watts) where
  toField = toField . uncompensated . unW

instance ToField (WattSeconds) where
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
  parseGraphSON = parseJSON . unwrapOne

instance FromGraphSON Watts where
  parseGraphSON = parseJSON . unwrapOne

-- Episodic Metrics

data Node a = Node
  { tx :: ! a
  , consumed :: !a
  , generated :: !a
  } deriving (Eq, Ord, Show, Generic, Functor, NFData, ToJSON, FromJSON)
  

instance (ToField a) => ToNamedRecord (Node a)

instance DefaultOrdered (Node a) where
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



txKey :: (FromJSON a, ToJSON a) => Key VFoundNode a
txKey = "tx"

consumedKey :: (FromJSON a, ToJSON a) => Key VFoundNode a
consumedKey = "consumed"

generatedKey :: (FromJSON a, ToJSON a) => Key VFoundNode a
generatedKey = "generated"

               
instance (GreskellC a) => NodeAttributes (Node a) where
  writeNodeAttributes node = fmap writeKeyValues $ sequence $
    [ txKey <=:> tx node
    , consumedKey <=:> consumed node
    , generatedKey <=:> generated node
    ]
  parseNodeAttributes props = pMapToFail (Node
                                          <$> lookupAs txKey props
                                          <*> lookupAs consumedKey props
                                          <*> lookupAs generatedKey props
                                         )

instance (GreskellC a) => FromGraphSON (Node a) where
  parseGraphSON = parseJSON . unwrapAll


data SensorMetrics e p = SensorMetrics
  { _time :: !(Maybe UTCTime)
  , lastTimeDiff :: !DiffTime
  , _powerT :: !(Node p)
  , _energyT :: !(Node e)
  , _battery :: !(Battery R R)
  , _demand :: !e
  } deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)


--instance (ToJSON e, ToJSON p) => ToJSON (SensorMetrics e p)

--instance (FromJSON e, FromJSON p) => ToJSON (SensorMetrics e p)

timeKey :: Key VFoundNode (Maybe UTCTime)
timeKey = "time"

timeDiffKey :: Key VFoundNode (DiffTime)
timeDiffKey = "timeDiff"

powerKey :: (GreskellC p) => Key VFoundNode (Node p)
powerKey = "power"

energyKey :: (GreskellC e) => Key VFoundNode (Node e)
energyKey = "energy"

batteryKey :: (GreskellC e, GreskellC p) => Key VFoundNode (Battery e p)
batteryKey = "battery"

demandKey :: (FromJSON a, ToJSON a) => Key VFoundNode a
demandKey = "demand"

instance (GreskellC e, GreskellC p) => NodeAttributes (SensorMetrics e p) where
  writeNodeAttributes SensorMetrics{..} = fmap writeKeyValues $ sequence $
    [ timeKey <=:> _time
    , timeDiffKey <=:> lastTimeDiff
    , powerKey <=:> _powerT
    , energyKey <=:> _energyT
    , batteryKey <=:> _battery
    , demandKey <=:> _demand
    ]
  parseNodeAttributes props = pMapToFail (SensorMetrics
                                          <$> lookupAs timeKey props
                                          <*> lookupAs timeDiffKey props
                                          <*> lookupAs powerKey props
                                          <*> lookupAs energyKey props
                                          <*> lookupAs batteryKey props
                                          <*> lookupAs demandKey props
                                         )


instance ToJSON (EnergyState) where
  toJSON a = object $ zipWith (A..=) esFieldNamesJSON (fieldAccessorsJSON a)
  toEncoding = messageToEncoding

instance FromJSON (EnergyState) where
  parseJSON (Object v) = do
    bv <- v .: "batteryV"
    gv <- v .: "gridV"
    b2l <- v .: "battery2LoadC"
    b2g <- v .: "battery2GridC"
    g2b <- v .: "grid2BatteryC"
    si <- v .: "solarC"
    return $ defMessage
                         & batteryVoltage .~ bv
                         & gridVoltage .~ gv
                         & batteryToLoadCurrent .~ b2l
                         & batteryToGridCurrent .~ b2g
                         & gridToBatteryCurrent .~ g2b
                         & solarInputCurrent .~ si
  parseJSON _ = mempty

esFieldNamesJSON :: [T.Text]
esFieldNamesJSON = ["batteryV",
                     "gridV",
                     "battery2LoadC",
                     "battery2GridC",
                     "grid2BatteryC",
                     "solarC"
                   ]

fieldAccessorsJSON es = es ^.. ( batteryVoltage
                          <> gridVoltage
                          <> batteryToLoadCurrent
                          <> batteryToGridCurrent
                          <> gridToBatteryCurrent
                          <> solarInputCurrent
                        )

esFieldNamesCSV :: [Name]
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

instance ToNamedRecord EnergyState where
  toNamedRecord es = HM.fromList $
                zip esFieldNamesCSV $
                map (pack . show) $
                fieldAccessorsCSV es


instance DefaultOrdered EnergyState where
  headerOrder _ = Vec.fromList esFieldNamesCSV

instance ToField UTCTime where
  toField t = pack (show t)

instance FromGraphSON UTCTime where
  parseGraphSON = parseJSON . unwrapOne

instance FromGraphSON DiffTime where
  parseGraphSON = parseJSON . unwrapOne

instance (ToField e, ToField p) => ToNamedRecord (SensorMetrics e p) where
  toNamedRecord (SensorMetrics {..}) = foldl (HM.union) (HM.fromList [("time", toField _time)])
    [ toNamedRecord _battery,
      toNamedRecord _powerT,
      toNamedRecord _energyT,
      --toNamedRecord _sensorsT,
      HM.fromList [("demand", toField _demand)]
    ]

showDec :: R -> String
showDec = (printf ("%.2g"))

prettyShow :: (Show e, Show p) => SensorMetrics e p -> String
prettyShow SensorMetrics{..} = ("last connection: " <> show _time)
    <> sep <> ("battery energy stored (Ws): " <> sep <> showDec (socPercentage _battery * totalCapacity _battery))
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


instance DefaultOrdered (SensorMetrics e p)

type Timestamp = (Maybe UTCTime, DiffTime)

data Battery e p = Battery
  { soc :: !e
  , chargeLim :: !p
  , dischargeLim :: !p
  , totalCapacity :: !e
  } deriving (Eq, Ord, Show, Generic, NFData)

instance (ToJSON e, ToJSON p) => ToJSON (Battery e p)
instance (FromJSON e, FromJSON p) => FromJSON (Battery e p)


instance (GreskellC e, GreskellC p) => FromGraphSON (Battery e p) where
  parseGraphSON = parseJSON . unwrapAll

socKey :: (FromJSON a, ToJSON a) => Key VFoundNode a
socKey = "soc"

chargeLimKey :: (FromJSON a, ToJSON a) => Key VFoundNode a
chargeLimKey = "chargeLim"

dischargeLimKey :: (FromJSON a, ToJSON a) => Key VFoundNode a
dischargeLimKey = "dischargeLim"

totalCapacityKey :: (FromJSON a, ToJSON a) => Key VFoundNode a
totalCapacityKey = "batteryCapacity"



instance (GreskellC e, GreskellC p) => NodeAttributes (Battery e p) where
  writeNodeAttributes b = fmap writeKeyValues $ sequence $
    [ socKey <=:> soc b
    , chargeLimKey <=:> chargeLim b
    , dischargeLimKey <=:> dischargeLim b
    , totalCapacityKey <=:> totalCapacity b
    ]
  parseNodeAttributes props = pMapToFail (Battery
                                          <$> lookupAs socKey props
                                          <*> lookupAs chargeLimKey props
                                          <*> lookupAs dischargeLimKey props
                                          <*> lookupAs totalCapacityKey props
                                         )


emptyB :: (Fractional e, Fractional p) => Battery e p
emptyB = Battery 0 0 0 0

instance DefaultOrdered (Battery e p)
instance (ToField e, ToField p) => ToNamedRecord (Battery e p)

instance (Fractional e, Fractional p, Ord e, Ord p) => Semigroup (Battery e p) where
  b <> b' = emptyB { soc = min (soc b)  (soc b')
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
      

socPercentage :: Fractional a => Battery a a -> a
socPercentage Battery{..} = (soc * 100 / totalCapacity)




{---------------------------------------------------------------------------------------------------------------------

                                          Helper Functions
---------------------------------------------------------------------------------------------------------------------}

type PowerN = Node (Watts)

type EnergyN = Node (WattSeconds)


storageSensors :: EnergyState -> SensorVector R
storageSensors es = SensorVector
  { sensorTerminalV = es ^. batteryVoltage
  , sensorCurrent =  i + o
  }
  where
    i = - (es ^. gridToBatteryCurrent + es ^. solarInputCurrent)
    o = es ^. batteryToGridCurrent + es ^. batteryToLoadCurrent

power :: EnergyState -> PowerN
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


utcTimeES :: EnergyState -> UTCTime
utcTimeES = utcTimeNow . (^. cpuTime)



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



-- Identified sensor type for monitoring
newtype TaggedNode n e p = TaggedNode (n, SensorMetrics e p) deriving (Generic)

instance (ToField n, ToField e, ToField p) => ToNamedRecord (TaggedNode n e p) where
  toNamedRecord (TaggedNode (n, ns)) = (HM.fromList [("NodeId", toField n)]) <> toNamedRecord ns

instance DefaultOrdered (TaggedNode n e p) where
  headerOrder _ = (Vec.fromList $ ["NodeId", "time"])
                  <> (headerOrder (undefined :: EnergyState))
                  <> (headerOrder (undefined :: PowerN))
                  <> (headerOrder (undefined :: EnergyN))
