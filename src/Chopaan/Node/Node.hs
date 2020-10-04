{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Chopaan.Node.Node (
  -- scans
  gridS, nodeS, energyS, powerS, timeS
  -- folds
  , energyFold, powerFold, timeFold
  -- data constructors
  , EnergyState, NodeS, NodeMetrics(..), Energy(..), Power(..), WattSeconds, Watts, Grid(..), pToE
  -- default builders
  , zeroMsg, defNodeS
  , nmFilter
  , writeCSVRecords
  -- initialization fns
  , toWattSeconds, toWatts
  ) where

import qualified Data.Time as Time
import Data.Time.Clock.POSIX

import GHC.Generics (Generic)

import Proto.NodeMessageSchema.NodeMessages
import Proto.NodeMessageSchema.NodeMessages_Fields hiding (time)

import Lens.Micro

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL


import Data.ProtoLens (defMessage)
import Data.ProtoLens.TextFormat
import Chopaan.Utils.JSON

import qualified Data.Map.Strict as Map
import Data.Function ((&))
import Data.Maybe (fromJust, isNothing, isJust)

import Data.Csv hiding ((.:))
import qualified Data.Vector as Vec (fromList)
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 (pack)
import System.IO
import System.Directory
import qualified Data.HashMap.Strict as HM

import Numeric.Compensated
import Control.Monad.State.Lazy

import Chopaan.Node.NodeId
import Chopaan.Node.Storage
import Chopaan.Utils.Time
import Data.Aeson hiding (encode, decode)
import qualified Data.Aeson as A
import Numeric.Estimator (KalmanFilter(..))

import Text.Printf
----------------------------------------------------------------------------------
-- Metric Tracking

-- Our Scalars

newtype WattSeconds = WS { unWs :: Compensated Double } deriving (Eq, Ord, Num, Generic, Fractional, Real, RealFrac)

newtype Watts = W { unW :: Compensated Double } deriving (Eq, Ord, Num, Generic, Fractional, Real, RealFrac)

instance Show WattSeconds where
  show = (printf ("%.2g")) . uncompensated . unWs

instance Show Watts where
  show = (printf ("%.2g")) . uncompensated . unW

type R = Double

toWatts :: Double -> Watts
toWatts a = W $ add a 0 compensated

toWattSeconds :: Double -> WattSeconds
toWattSeconds a = WS $ add a 0 compensated


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

--instance (Show a) => Show (Energy a) where
--  show (Energy{..}) = "Energy : "

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
  { _time :: !(Maybe Time.UTCTime)
  , lastTimeDiff :: !Time.DiffTime
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

instance ToField Time.UTCTime where
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
    <> sep <> ("SoC Percentage: " <> sep <> showDec (socPercentage _battery))
    <> sep <> ("Runtime Estimate: " <> sep <> showDec (secsToMinutes $ runTime @R _battery (storageSensors _sensorsT)))
    <> sep <> ("current demand (Ws): " <> show _demand)
    <> sep <> ("current power:" <> sep <> show _powerT)
    <> sep <> ("current energy:" <> sep <> show _energyT)
    <> sep <> ("sensor readings:" <> sep <> (show (pprintMessage _sensorsT)))
    where
      sep = "\n"

secsToMinutes :: (Num a) => a -> a
secsToMinutes = (* 60)

nmFilter :: (NodeId a) -> NodeS -> Bool
nmFilter _ = isJust . _time

type NodeS = NodeMetrics WattSeconds Watts 

instance DefaultOrdered (NodeMetrics e p)

type Timestamp = (Maybe Time.UTCTime, Time.DiffTime)


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

defNodeS :: NodeS
defNodeS = NodeMetrics Nothing 0 mempty mempty zeroMsg mempty 0


{----------------------------------------------------------------------------------------------------


Folds of type FL.Fold, as functions to the instantatenous values of the system over an indexed set.
                      :: forall s. Fold (s -> a -> m s) (m s) (s -> m b)


-----------------------------------------------------------------------------------------------------}

timeFold :: forall m. Monad m => FL.Fold m (EnergyState) Timestamp
timeFold = FL.Fold step' begin' done'
  where
    step' :: (Timestamp -> EnergyState -> m Timestamp)
    step' (Nothing, _) cur = pure (Just tn, diffUTC tn tn)
      where
        tn = utcTimeES cur
    step' ((Just !prev), _) cur = pure (Just tn, diffUTC tn prev)
      where
        tn = utcTimeES cur
    begin' :: m Timestamp
    begin' = pure (Nothing, 0)
    done' :: Timestamp -> m Timestamp
    done' = pure



powerFold :: forall m. (Monad m) => FL.Fold m EnergyState (Power Watts)
powerFold = FL.Fold (\_ b-> pure $ power b) (pure $ mempty) return 


energyFold :: forall m. (Monad m) => FL.Fold m (EnergyState) (Energy WattSeconds)
energyFold = (FL.Fold step begin end)
  where
    -- forall s. Fold (s -> a -> m s) (m s) (s -> m b)
    step :: (Energy WattSeconds, Maybe Time.UTCTime) -> EnergyState -> m (Energy WattSeconds, Maybe Time.UTCTime)
    step (esPrev, (Just tPrev)) cur = pure $ ((esPrev <> (eAtT (power cur) (Just tn, diffUTC tn tPrev))), Just tn)
      where
        tn = utcTimeES cur
    step (esPrev, Nothing) cur = pure $ ((esPrev <> (eAtT (power cur) (Just tn, diffUTC tn tn))), Just tn)
      where
        tn = utcTimeES cur
    begin :: m (Energy WattSeconds, Maybe Time.UTCTime)
    begin = pure $ (mempty, Nothing)
    end :: (Energy WattSeconds, Maybe Time.UTCTime) -> m (Energy WattSeconds)
    end = pure . fst
    eAtT :: Power Watts -> Timestamp -> (Energy WattSeconds)
    eAtT p (_, t) = Energy { txIn = (pToE t tInP)
                           , txOut = (pToE t tOutP)
                           , consumed = (pToE t loadP)
                           , generated = (pToE t genP)}
      where
        Power{..} = p

pToE :: (Real t) => t -> Watts -> WattSeconds
pToE t (W p') = WS $ (realToFrac t) *^ p'

batteryFold :: forall m. (Monad m) => BatteryParams R -> FL.Fold m EnergyState (Battery R R)
batteryFold bat@BatteryParams{..} = FL.Fold step begin end
  where
    step :: (Maybe Time.UTCTime, Maybe (KF R)) -> EnergyState -> m (Maybe Time.UTCTime, Maybe (KF R))
    step (t, kf) sensorReadings = ((\(_, b) -> (Just tnow, Just b)) . snd) <$>
        (runKalmanState (tdiff t) (cState kf) $
        runProcessModel bat (tdiff t) processNoise sensorNoise $
        storageSensors sensorReadings)
      where
        cState (Just (KalmanFilter currState _)) = currState
        cState Nothing = initDynamic {soC = ocvToSoC bat (sensorTerminalV . storageSensors $ sensorReadings)}
        tnow = utcTimeES sensorReadings
        tdiff (Just t') = realToFrac $ Time.diffUTCTime tnow t'
        tdiff Nothing = 0
        
    begin :: m (Maybe Time.UTCTime, Maybe (KF R))
    begin = return $ (Nothing, Nothing)
    end :: (Maybe Time.UTCTime, Maybe (KF R)) -> m (Battery R R)
    end (_, Just (KalmanFilter (StateVector{..}) _)) = return $ (emptyB @R @R) { soc = soC
                                                                               , totalCapacity = chargeCapacity}
    end (_, Nothing) = return $ emptyB @R @R


nodeMonitor :: forall m. (Monad m) => FL.Fold m (EnergyState) (NodeMetrics WattSeconds Watts) 
nodeMonitor = NodeMetrics <$> (fst <$> tn) <*> (snd <$> tn) <*> powerFold <*> en <*> sensors <*> (batteryFold defBatteryParams) <*> demandFold 
  where
    tn :: FL.Fold m (EnergyState) Timestamp
    tn = timeFold
    en :: FL.Fold m (EnergyState) (Energy WattSeconds)
    en =  energyFold
    sensors :: FL.Fold m (EnergyState) (EnergyState)
    sensors = FL.Fold (\_ nes -> pure nes) (pure zeroMsg) (pure) 
    demandFold :: FL.Fold m (EnergyState) WattSeconds
    demandFold = FL.Fold (\_ nes -> pure (d $ power nes)) (pure 0) pure
      where
        d (Power{..}) = pToE (60 * 10) loadP



{--------------------------------------------------------------------------------------------------------------

                                          Streams of Folds
---------------------------------------------------------------------------------------------------------------}


energyS :: (MonadAsync m, IsStream t) => t m EnergyState -> t m (Energy WattSeconds)
energyS = S.postscan energyFold

timeS :: (MonadAsync m, IsStream t) => t m EnergyState -> t m Timestamp
timeS = S.postscan timeFold

powerS :: (MonadAsync m, IsStream t) => t m EnergyState -> t m (Power Watts)
powerS = S.postscan powerFold

nodeS :: (MonadAsync m, IsStream t) => t m EnergyState -> t m NodeS
nodeS = S.postscan nodeMonitor

newtype Grid n s = Grid (Map.Map n s) deriving (Show, Generic, Functor)

type GridS n = Grid n NodeS

gridS :: forall t m n . (IsStream t, MonadAsync m, Ord n) => [n] -> t m (n, EnergyState) -> t m (GridS n)
gridS ns ss = S.postscan gridMap $ ss
  where
    gridMap = Grid <$> FL.demux nodeMap
      where
        nodeMap = Map.fromList $ zip ns $ repeat nodeMonitor 

{----------------------------------------------------------

                CSV Conversion
----------------------------------------------------------}

newtype TaggedNode n = TaggedNode (n, NodeS) deriving (Generic)

instance (ToField n) => ToNamedRecord (TaggedNode n) where
  toNamedRecord (TaggedNode (n, ns)) = (HM.fromList [("NodeId", toField n)]) <> toNamedRecord ns

instance DefaultOrdered (TaggedNode n) where
  headerOrder _ = (Vec.fromList $ ["NodeId", "time"])
                  <> (headerOrder (undefined :: EnergyState))
                  <> (headerOrder (undefined :: Power Watts))
                  <> (headerOrder (undefined :: Energy WattSeconds))

--TODO: Generalize This


writeCSVRecords :: forall n. (ToField n)
  => FilePath
  -> GridS n
  ->  StateT Time.UTCTime IO ()
writeCSVRecords fp (Grid gs) = do
  fE <- liftIO $ doesFileExist fp
  lastWrite <- get
  let
    opts = if fE then contOpts else initOpts
    recs = atT gs lastWrite
    maxWrite = maxWriteT recs
  liftIO $ withFile fp AppendMode $ (\ho ->do
      (BSL.hPut ho) $ encodeDefaultOrderedByNameWith opts recs)
  put $ maxWrite
  where
    contOpts = defaultEncodeOptions {
      encUseCrLf = True,
      encIncludeHeader = False
    }
    initOpts = contOpts { encIncludeHeader = True }
    maxWriteT :: [TaggedNode n] -> Time.UTCTime
    maxWriteT ts = maximum tMlist
      where
        tMlist :: [Time.UTCTime]
        tMlist = map fromJust $ filter isNothing $ (\(TaggedNode(_, NodeMetrics {_time})) -> _time) <$> ts
    atT :: Map.Map n (NodeS) -> Time.UTCTime -> [TaggedNode n]
    atT nmap lw = TaggedNode <$> (timeFilter lw $ Map.toList nmap)
    timeFilter lw = (filter (\(_, NodeMetrics {_time}) -> tf _time))
      where
        tf Nothing = False
        tf (Just t) = t > lw


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

power :: EnergyState -> Power Watts
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
    p :: Getting Double EnergyState Double -> Getting Double EnergyState Double -> Watts
    p v i = W $ (es ^. i) *^ v'
      where
        v' = add (es ^. v) 0 compensated

utcTimeES :: EnergyState -> Time.UTCTime
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
