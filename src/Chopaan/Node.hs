{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Chopaan.Node (
  -- scans
  gridS, nodeS, energyS, powerS, timeS
  -- folds
  , energyFold, powerFold, timeFold
  -- data constructors
  , EnergyState, NodeId(..), NodeS, NodeMetrics(..), Energy(..), Power(..), WattSeconds, Watts, Grid(..), pToE
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

import Proto.NodeMessages
import Proto.NodeMessages_Fields hiding (time)

import Lens.Micro

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL


import Data.ProtoLens (defMessage)
import Data.ProtoLens.TextFormat

import Data.Hashable
import qualified Data.Map.Strict as Map
import Data.Function ((&))
import Data.Maybe (fromJust, isNothing, isJust)

import Data.Csv
import qualified Data.Vector as Vec (fromList)
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 (pack)
import System.IO
import System.Directory
import qualified Data.HashMap.Strict as HM

import Numeric.Compensated
import Control.Monad.State.Lazy
import Numeric.Estimator (KalmanFilter(..))

import Chopaan.Storage


----------------------------------------------------------------------------------
-- Metric Tracking

-- Our Scalars

type WattSeconds = Compensated Double

type Watts = Compensated Double

type R = Double

toWatts :: Double -> Watts
toWatts a = add a 0 compensated

toWattSeconds :: Double -> WattSeconds
toWattSeconds a = add a 0 compensated

  
instance ToField (Compensated Double) where
  toField = toField . uncompensated 

newtype NodeId a = NodeId { unNodeId :: a } deriving (Eq, Show, Ord, Generic)

instance (Hashable a) => Hashable (NodeId a) where
  hashWithSalt n (NodeId a) = hashWithSalt n a


instance (ToField a) => ToField (NodeId a) where
  toField (NodeId a) = toField a 

-- Episodic Metrics

data Energy a = Energy
  { txIn :: !a
  , txOut :: !a
  , consumed :: !a
  , generated :: !a
  } deriving (Eq, Ord, Generic, Functor)

instance (Show a) => Show (Energy a) where
  show Energy{..} = "Energy Balance (Wattseconds)" <> nl
    <> "Generated : " <> (rs generated) <> nl
    <> "Consumed : " <> (rs consumed) <> nl
    <> "Incoming : " <> (rs txIn) <> nl
    <> "Outgoing : " <> (rs txOut) <> nl
    where
      nl = "\n"
      rs = show

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
  } deriving (Eq, Ord, Generic)



instance ToNamedRecord EnergyState where
  toNamedRecord es = HM.fromList $
                zip names $
                map (pack . show) $
                es ^.. ( batteryVoltage
                         <> gridVoltage
                         <> batteryToLoadCurrent
                         <> batteryToGridCurrent
                         <> gridToBatteryCurrent
                         <> solarInputCurrent
                         <> dutyCycle
                         -- <> cpuTime
                       )
                where
                  names = ["batteryV",
                           "gridV",
                           "battery2LoadC",
                           "battery2GridC",
                           "grid2BatteryC",
                           "solarC",
                           "dutyC"]--,
                           --"cpuTime"]


instance DefaultOrdered EnergyState where
  headerOrder _ = Vec.fromList $ names
    where
      names = [ "batteryV",
                "gridV",
                "battery2LoadC",
                "battery2GridC",
                "grid2BatteryC",
                "solarC",
                "dutyC"]

instance ToField Time.UTCTime where
  toField t = pack (show t)

instance (ToField e, ToField p) => ToNamedRecord (NodeMetrics e p) where
  toNamedRecord (NodeMetrics {..}) = foldl (HM.union) (HM.fromList [("time", toField _time)])
    [ toNamedRecord _battery,
      toNamedRecord _powerT,
      toNamedRecord _energyT --,
      --toNamedRecord _sensorsT
    ]

instance (Show e, Show p, RealFrac e, RealFrac p) => Show (NodeMetrics e p) where
  show NodeMetrics{..} = ("last connection: " <> show _time)
    <> sep <> ("SoC Percentage: " <> sep <> show (socPercentage _battery))
    <> sep <> ("Runtime Estimate: " <> sep <> show (secsToMinutes $ runTime @R _battery (storageSensors _sensorsT)))
    -- <> sep <> ("current demand (Ws): " <> show _demand)
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

instance DefaultOrdered (NodeMetrics p e)

type Timestamp = (Maybe Time.UTCTime, Time.DiffTime)


data Battery e p = Battery
  { soc :: !e
  , chargeLim :: !p
  , dischargeLim :: !p
  , totalCapacity :: !e
  } deriving (Eq, Ord, Show, Generic)

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
defNodeS = NodeMetrics Nothing 0 mempty mempty zeroMsg mempty


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
        tn = utcTimeNow cur
    step' ((Just !prev), _) cur = pure (Just tn, diffUTC tn prev)
      where
        tn = utcTimeNow cur
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
        tn = utcTimeNow cur
    step (esPrev, Nothing) cur = pure $ ((esPrev <> (eAtT (power cur) (Just tn, diffUTC tn tn))), Just tn)
      where
        tn = utcTimeNow cur
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
pToE t p' = (realToFrac t) *^ p'

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
        tnow = utcTimeNow sensorReadings
        tdiff (Just t') = realToFrac $ Time.diffUTCTime tnow t'
        tdiff Nothing = 0
        
    begin :: m (Maybe Time.UTCTime, Maybe (KF R))
    begin = return $ (Nothing, Nothing)
    end :: (Maybe Time.UTCTime, Maybe (KF R)) -> m (Battery R R)
    end (_, Just (KalmanFilter (StateVector{..}) _)) = return $ (emptyB @R @R) { soc = soC
                                                                               , totalCapacity = chargeCapacity}
    end (_, Nothing) = return $ emptyB @R @R


nodeMonitor :: forall m. (Monad m) => FL.Fold m (EnergyState) (NodeMetrics Watts WattSeconds) 
nodeMonitor = NodeMetrics <$> (fst <$> tn) <*> (snd <$> tn) <*> powerFold <*> en <*> sensors <*> (batteryFold defBatteryParams) 
  where
    tn :: FL.Fold m (EnergyState) Timestamp
    tn = timeFold
    en :: FL.Fold m (EnergyState) (Energy WattSeconds)
    en =  energyFold
    sensors :: FL.Fold m (EnergyState) (EnergyState)
    sensors = FL.Fold (\_ nes -> pure nes) (pure zeroMsg) (pure) 




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
    p v i = (es ^. i) *^ v'
      where
        v' = add (es ^. v) 0 compensated

--errorHandle :: (Num a, Compensable a) =>  a -> a -> Compensated a
--errorHandle a b = a


-- $ converts the millisecond timestamp in the EnergyState to a UTCTime  
utcTimeNow :: EnergyState -> Time.UTCTime
utcTimeNow es = posixSecondsToUTCTime $ (fromIntegral $ (es ^. cpuTime))

diffUTC :: Time.UTCTime -> Time.UTCTime -> Time.DiffTime
diffUTC a b = realToFrac $ Time.diffUTCTime a b

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
