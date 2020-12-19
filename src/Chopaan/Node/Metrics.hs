{-# LANGUAGE DeriveGeneric, RecordWildCards, NamedFieldPuns, TypeApplications, DeriveFunctor, OverloadedStrings, FlexibleContexts #-}
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

import Data.ProtoLens
import Data.ProtoLens.TextFormat
import Lens.Micro
import Text.Printf


import Chopaan.Node.NodeId
import Chopaan.Node.Storage
import Chopaan.Utils.JSON
import Chopaan.Utils.Time


import Proto.NodeMessageSchema.NodeMessages hiding (NodeId)
import Proto.NodeMessageSchema.NodeMessages_Fields
import ConCat.Misc (R)

-- Episodic Metrics

data Energy a = Energy
  { txIn :: !a
  , txOut :: !a
  , consumed :: !a
  , generated :: !a
  } deriving (Eq, Ord, Generic, Functor)

instance (ToJSON a) => ToJSON (Energy a)
instance (FromJSON a) => FromJSON (Energy a)


instance (Show a) => Show (Energy a) where
  show Energy{..} = "Energy" <> nl
    <> "Generated : " <> (rs generated)
    <> "Consumed : " <> (rs consumed)
    <> "Incoming : " <> (rs txIn)
    <> "Outgoing : " <> (rs txOut)
    where
      nl = "\n"
      rs x = show x <> nl

instance (ToField a) => ToNamedRecord (Energy a)

instance DefaultOrdered (Energy a) where
  headerOrder _ = Vec.fromList ["txIn", "txOut", "consumed", "generated"]

instance Applicative Energy where
  pure v = Energy
    { txIn = v
    , txOut = v
    , consumed = v
    , generated = v
    }
  f <*> v = Energy
              { txIn = txIn f $ txIn v
              , txOut = txOut f $ txOut v
              , consumed = consumed f $ consumed v
              , generated = generated f $ generated v
              }

initEA :: (Num a) => Energy a
initEA = Energy 0 0 0 0

-- Check associativity
instance (Num a) => Semigroup (Energy a) where
  v1 <> v2 = Energy
    { txIn = txIn v1 + txIn v2
    , txOut = txOut v1 + txOut v2
    , consumed = consumed v1 + consumed v2
    , generated = generated v1 + generated v2
    }

instance (Num a) => Monoid (Energy a) where
  mempty = initEA



data Power a = Power
  { genP :: !a
  , tInP :: !a
  , tOutP :: !a
  , loadP :: !a }
  deriving (Eq, Ord, Generic, Functor)


instance (Show a) => Show (Power a) where
  show Power{..} = "Power (Watts)" <> nl
    <> "Generation : " <> (rs genP) <> nl
    <> "Load : " <> (rs loadP) <> nl
    <> "Incoming : " <> (rs tInP) <> nl
    <> "Outgoing : " <> (rs tOutP) <> nl
    where
      nl = "\n"
      rs = show

instance (ToJSON a) => ToJSON (Power a)
instance (FromJSON a) => FromJSON (Power a)

instance (ToField a) => ToNamedRecord (Power a)

instance DefaultOrdered (Power a) where
  headerOrder _ = Vec.fromList ["genP", "tInP", "tOutP", "loadP"]

instance Applicative Power where
  pure v = Power
    { tInP = v
    , tOutP = v
    , loadP = v
    , genP = v
    }
  f <*> v = Power
              { tInP = tInP f $ tInP v
              , tOutP = tOutP f $ tOutP v
              , loadP = loadP f $ loadP v
              , genP = genP f $ genP v
              }

instance (Num a) => Semigroup (Power a) where
  p <> p' = (+) <$> p <*> p'

instance (Num a) => Monoid (Power a) where
  mempty = Power 0 0 0 0

--instance (Num a) => VS.V R (Power a) where

data NodeMetrics e p = NodeMetrics
  { _time :: !(Maybe UTCTime)
  , lastTimeDiff :: !DiffTime
  , _powerT :: !(Power p)
  , _energyT :: !(Energy e)
  , _sensorsT :: !EnergyState
  , _battery :: !(Battery R R)
  , _demand :: !e
  } deriving (Eq, Ord, Generic)


instance (ToJSON e, ToJSON p) => ToJSON (NodeMetrics e p)
--instance (FromJSON e, FromJSON p) => FromJSON (NodeMetrics e p)

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

instance (ToField e, ToField p) => ToNamedRecord (NodeMetrics e p) where
  toNamedRecord (NodeMetrics {..}) = foldl (HM.union) (HM.fromList [("time", toField _time)])
    [ toNamedRecord _battery,
      toNamedRecord _powerT,
      toNamedRecord _energyT,
      toNamedRecord _sensorsT,
      HM.fromList [("demand", toField _demand)]
    ]

showDec = (printf ("%.2g"))

instance (Show e, Show p, RealFrac e, RealFrac p) => Show (NodeMetrics e p) where
  show NodeMetrics{..} = ("last connection: " <> show _time)
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

nmFilter :: (NodeId a) -> NodeMetrics e p -> Bool
nmFilter _ = isJust . _time



instance DefaultOrdered (NodeMetrics e p)

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


storageSensors :: EnergyState -> SensorVector R
storageSensors es = SensorVector
  { sensorTerminalV = es ^. batteryVoltage
  , sensorCurrent =  i + o
  }
  where
    i = - (es ^. gridToBatteryCurrent + es ^. solarInputCurrent)
    o = es ^. batteryToGridCurrent + es ^. batteryToLoadCurrent

power :: EnergyState -> Power Double
power es = Power
           { tInP = txIn'
           , tOutP = txOut'
           , loadP = cnsm'
           , genP = genP' }
  where
    txIn' = p batteryVoltage gridToBatteryCurrent
    txOut' = p batteryVoltage batteryToGridCurrent
    cnsm' = p batteryVoltage batteryToLoadCurrent
    genP' = p batteryVoltage solarInputCurrent
    p v i = (es ^. i) * v'
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



-- Identified node type for monitoring
newtype TaggedNode n e p = TaggedNode (n, NodeMetrics e p) deriving (Generic)

instance (ToField n, ToField e, ToField p) => ToNamedRecord (TaggedNode n e p) where
  toNamedRecord (TaggedNode (n, ns)) = (HM.fromList [("NodeId", toField n)]) <> toNamedRecord ns

instance DefaultOrdered (TaggedNode n e p) where
  headerOrder _ = (Vec.fromList $ ["NodeId", "time"])
                  <> (headerOrder (undefined :: EnergyState))
                  <> (headerOrder (undefined :: Power p))
                  <> (headerOrder (undefined :: Energy e))

