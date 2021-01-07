{-# LANGUAGE RecordWildCards, NamedFieldPuns, TypeApplications, DeriveFunctor, OverloadedStrings, FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Chopaan.Node.Metrics where

import GHC.Generics hiding (R)

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
import Data.ProtoLens.TextFormat
import Lens.Micro
import Text.Printf


import Chopaan.Node.NodeId
import Chopaan.Node.Storage
import Chopaan.Utils.JSON
import Chopaan.Utils.Time


import Proto.NodeMessageSchema.NodeMessages hiding (SensorId)
import Proto.NodeMessageSchema.NodeMessages_Fields
import ConCat.Misc (R)

{----- Basic Types ------}

newtype WattSeconds = WS { unWs :: Compensated Double } deriving (Eq, Ord, Num, Generic, Fractional, Real, RealFrac)

newtype Watts = W { unW :: Compensated Double } deriving (Eq, Ord, Num, Generic, Fractional, Real, RealFrac)

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



-- Episodic Metrics

data Node a = Node
  { txIn :: !a
  , txOut :: !a
  , consumed :: !a
  , generated :: !a
  } deriving (Eq, Ord, Generic, Functor)

instance (ToJSON a) => ToJSON (Node a)
instance (FromJSON a) => FromJSON (Node a)


instance (Show a) => Show (Node a) where
  show Node{..} = 
    "Generated : " <> (rs generated)
    <> "Consumed : " <> (rs consumed)
    <> "Incoming : " <> (rs txIn)
    <> "Outgoing : " <> (rs txOut)
    where
      nl = "\n"
      rs x = show x <> nl

instance (ToField a) => ToNamedRecord (Node a)

instance DefaultOrdered (Node a) where
  headerOrder _ = Vec.fromList ["txIn", "txOut", "consumed", "generated"]

instance Applicative Node where
  pure v = Node
    { txIn = v
    , txOut = v
    , consumed = v
    , generated = v
    }
  f <*> v = Node
              { txIn = txIn f $ txIn v
              , txOut = txOut f $ txOut v
              , consumed = consumed f $ consumed v
              , generated = generated f $ generated v
              }

initEA :: (Num a) => Node a
initEA = Node 0 0 0 0

-- Check associativity
instance (Num a) => Semigroup (Node a) where
  v1 <> v2 = Node
    { txIn = txIn v1 + txIn v2
    , txOut = txOut v1 + txOut v2
    , consumed = consumed v1 + consumed v2
    , generated = generated v1 + generated v2
    }

instance (Num a) => Monoid (Node a) where
  mempty = initEA





--instance (Num a) => VS.V R (Power a) where

data SensorMetrics e p = SensorMetrics
  { _time :: !(Maybe UTCTime)
  , lastTimeDiff :: !DiffTime
  , _powerT :: !(Node p)
  , _energyT :: !(Node e)
  , _sensorsT :: !EnergyState
  , _battery :: !(Battery R R)
  , _demand :: !e
  } deriving (Eq, Ord, Generic)


instance (ToJSON e, ToJSON p) => ToJSON (SensorMetrics e p)
--instance (FromJSON e, FromJSON p) => FromJSON (SensorMetrics e p)

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

instance (ToField e, ToField p) => ToNamedRecord (SensorMetrics e p) where
  toNamedRecord (SensorMetrics {..}) = foldl (HM.union) (HM.fromList [("time", toField _time)])
    [ toNamedRecord _battery,
      toNamedRecord _powerT,
      toNamedRecord _energyT,
      toNamedRecord _sensorsT,
      HM.fromList [("demand", toField _demand)]
    ]

showDec = (printf ("%.2g"))

instance (Show e, Show p, RealFrac e, RealFrac p) => Show (SensorMetrics e p) where
  show SensorMetrics{..} = ("last connection: " <> show _time)
    <> sep <> ("battery energy stored (Ws): " <> sep <> showDec (socPercentage _battery * totalCapacity _battery))
    <> sep <> ("runtime estimate :" <> sep <> showDec (secsToMinutes $ runTime @R _battery (storageSensors _sensorsT)))
    <> sep <> ("current demand (Ws): " <> show _demand)
    <> sep <> ("current power:" <> sep <> show _powerT)
    <> sep <> ("current energy:" <> sep <> show _energyT)
    <> sep <> ("sensor readings:" <> sep <> (show (pprintMessage _sensorsT)))
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
  } deriving (Eq, Ord, Show, Generic)

instance (ToJSON e, ToJSON p) => ToJSON (Battery e p)
instance (FromJSON e, FromJSON p) => FromJSON (Battery e p)

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

type Power = Node (Watts)

type Energy = Node (WattSeconds)


storageSensors :: EnergyState -> SensorVector R
storageSensors es = SensorVector
  { sensorTerminalV = es ^. batteryVoltage
  , sensorCurrent =  i + o
  }
  where
    i = - (es ^. gridToBatteryCurrent + es ^. solarInputCurrent)
    o = es ^. batteryToGridCurrent + es ^. batteryToLoadCurrent

power :: EnergyState -> Power
power es = Node
           { txIn = txIn'
           , txOut = txOut'
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
                  <> (headerOrder (undefined :: Power))
                  <> (headerOrder (undefined :: Energy))



