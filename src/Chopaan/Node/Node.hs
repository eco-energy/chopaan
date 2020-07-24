{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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
  {--, NodeSensors, NodeId(..), NodeS, NodeMetrics(..), Energy(..), Power(..), WattSeconds, Watts, Grid(..), pToE
  -- default builders
  , zeroMsg, defNodeS
  , nmFilter
  , writeToCSVRecords
  -- initialization fns
  , toWattSeconds, toWatts--}
  ) where

import qualified Data.Time as Time
import GHC.Generics (Generic)

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL


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
import Chopaan.NodeSensors

import Data.Aeson


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

instance ToJSON (Compensated Double) where
  toJSON = toJSON . uncompensated

instance FromJSON (Compensated Double) where
  parseJSON b = (\a -> add a 0 compensated) <$> (parseJSON @Double b)

newtype NodeId a = NodeId { unNodeId :: a } deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

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
  } deriving (Eq, Ord, Generic, Functor, ToJSON, FromJSON)

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

data NodePower a = NodePower
  { genP :: !a
  , tInP :: !a
  , tOutP :: !a
  , loadP :: !a }
  deriving (Eq, Ord, Generic, Functor, ToJSON, FromJSON)




instance (Show a) => Show (NodePower a) where
  show NodePower{..} = "NodePower (Watts)" <> nl
    <> "Generation : " <> rs genP <> nl
    <> "Load : " <> rs loadP <> nl
    <> "Incoming : " <> rs tInP <> nl
    <> "Outgoing : " <> rs tOutP <> nl
    where
      nl = "\n"
      rs = show

instance (ToField a) => ToNamedRecord (NodePower a)

instance DefaultOrdered (NodePower a) where
  headerOrder _ = Vec.fromList ["genP", "tInP", "tOutP", "loadP"]

instance Applicative NodePower where
  pure v = NodePower
    { tInP = v
    , tOutP = v
    , loadP = v
    , genP = v
    }
  f <*> v = NodePower
              { tInP = tInP f $ tInP v
              , tOutP = tOutP f $ tOutP v
              , loadP = loadP f $ loadP v
              , genP = genP f $ genP v
              }

instance (Num a) => Semigroup (NodePower a) where
  p <> p' = (+) <$> p <*> p'

instance (Num a) => Monoid (NodePower a) where
  mempty = NodePower 0 0 0 0


data NodeMetrics e p = NodeMetrics
  { _time :: !(Maybe Time.UTCTime)
  , lastTimeDiff :: !Time.DiffTime
  , _powerT :: !(NodePower p)
  , _energyT :: !(Energy e)
  , _battery :: !(Battery R R)
  } deriving (Eq, Ord, Generic, ToJSON, FromJSON)



instance ToNamedRecord NodeSensors s where
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


instance DefaultOrdered NodeSensors s where
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
  toNamedRecord NodeMetrics {..} = foldl HM.union (HM.fromList [("time", toField _time)])
    [ toNamedRecord _battery,
      toNamedRecord _powerT,
      toNamedRecord _energyT --,
      --toNamedRecord _sensorsT
    ]

instance (Show e, Show p, RealFrac e, RealFrac p) => Show (NodeMetrics e p) where
  show NodeMetrics{..} = ("last connection: " <> show _time)
    <> sep <> ("SoC Percentage: " <> sep <> show (socPercentage _battery))
--    <> sep <> ("Runtime Estimate: " <> sep <> show (secsToMinutes $ runTime @R _battery (storageSensors _sensorsT)))
    -- <> sep <> ("current demand (Ws): " <> show _demand)
    <> sep <> ("current power:" <> sep <> show _powerT)
    <> sep <> ("current energy:" <> sep <> show _energyT)
--    <> sep <> ("sensor readings:" <> sep <> (show (pprintMessage _sensorsT)))
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
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

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
runTime Battery{soc} SensorVector{..} = soc / normC sensorCurrent * sensorTerminalV
  where
    normC c
      | c >= 0 = c
      | c < 0 = 0.05
      | otherwise = error "neither greater nor less than nor equal to zero"
      

socPercentage :: Fractional a => Battery a a -> a
socPercentage Battery{..} = soc * 100 / totalCapacity

defNodeS :: NodeS
defNodeS = NodeMetrics Nothing 0 mempty mempty mempty


{----------------------------------------------------------------------------------------------------


Folds of type FL.Fold, as functions to the instantatenous values of the system over an indexed set.
                      :: forall s. Fold (s -> a -> m s) (m s) (s -> m b)


-----------------------------------------------------------------------------------------------------}

timeFold :: forall m. Monad m => FL.Fold m NodeSensors s Timestamp
timeFold = FL.Fold step' begin' done'
  where
    step' :: (Timestamp -> NodeSensors s -> m Timestamp)
    step' (Nothing, _) cur = pure (Just tn, diffUTC tn tn)
      where
        tn = utcTimeNow cur
    step' (Just !prev, _) cur = pure (Just tn, diffUTC tn prev)
      where
        tn = utcTimeNow cur
    begin' :: m Timestamp
    begin' = pure (Nothing, 0)
    done' :: Timestamp -> m Timestamp
    done' = pure



powerFold :: forall m. (Monad m) => FL.Fold m NodeSensors s (NodePower Watts)
powerFold = FL.Fold (\_ b-> pure $ power b) (pure mempty) return 


energyFold :: forall m. (Monad m) => FL.Fold m NodeSensors s (Energy WattSeconds)
energyFold = FL.Fold step begin end
  where
    -- forall s. Fold (s -> a -> m s) (m s) (s -> m b)
    step :: (Energy WattSeconds, Maybe Time.UTCTime) -> NodeSensors s -> m (Energy WattSeconds, Maybe Time.UTCTime)
    step (esPrev, Just tPrev) cur = pure (esPrev <> eAtT (power cur) (Just tn, diffUTC tn tPrev), Just tn)
      where
        tn = utcTimeNow cur
    step (esPrev, Nothing) cur = pure (esPrev <> eAtT (power cur) (Just tn, diffUTC tn tn), Just tn)
      where
        tn = utcTimeNow cur
    begin :: m (Energy WattSeconds, Maybe Time.UTCTime)
    begin = pure (mempty, Nothing)
    end :: (Energy WattSeconds, Maybe Time.UTCTime) -> m (Energy WattSeconds)
    end = pure . fst
    eAtT :: NodePower Watts -> Timestamp -> Energy WattSeconds
    eAtT p (_, t) = Energy { txIn = pToE t tInP
                           , txOut = pToE t tOutP
                           , consumed = pToE t loadP
                           , generated = pToE t genP
                           }
      where
        NodePower{..} = p

pToE :: (Real t) => t -> Watts -> WattSeconds
pToE t p' = realToFrac t *^ p'

batteryFold :: forall m. (Monad m) => BatteryParams R -> FL.Fold m NodeSensors s (Battery R R)
batteryFold bat@BatteryParams{..} = FL.Fold step begin end
  where
    step :: (Maybe Time.UTCTime, Maybe (KF R)) -> NodeSensors s -> m (Maybe Time.UTCTime, Maybe (KF R))
    step (t, kf) sensorReadings = (\(_, b) -> (Just tnow, Just b)) . snd <$>
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
    begin = return (Nothing, Nothing)
    end :: (Maybe Time.UTCTime, Maybe (KF R)) -> m (Battery R R)
    end (_, Just (KalmanFilter StateVector{..} _)) = return $ (emptyB @R @R) { soc = soC
                                                                               , totalCapacity = chargeCapacity}
    end (_, Nothing) = return $ emptyB @R @R


nodeMonitor :: forall m. (Monad m) => FL.Fold m (NodeSensors s) (NodeMetrics Watts WattSeconds) 
nodeMonitor = NodeMetrics <$> (fst <$> tn) <*> (snd <$> tn) <*> powerFold <*> en <*> batteryFold defBatteryParams 
  where
    tn :: FL.Fold m NodeSensors s Timestamp
    tn = timeFold
    en :: FL.Fold m NodeSensors s (Energy WattSeconds)
    en =  energyFold
    --sensors :: FL.Fold m (NodeSensors s) (NodeSensors s)
    --sensors = FL.Fold (\_ nes -> pure nes) (pure zeroMsg) (pure) 




{--------------------------------------------------------------------------------------------------------------

                                          Streams of Folds
---------------------------------------------------------------------------------------------------------------}


energyS :: (MonadAsync m, IsStream t) => t m NodeSensors s -> t m (Energy WattSeconds)
energyS = S.postscan energyFold

timeS :: (MonadAsync m, IsStream t) => t m NodeSensors s -> t m Timestamp
timeS = S.postscan timeFold

powerS :: (MonadAsync m, IsStream t) => t m NodeSensors s -> t m (NodePower Watts)
powerS = S.postscan powerFold

nodeS :: (MonadAsync m, IsStream t) => t m NodeSensors s -> t m NodeS
nodeS = S.postscan nodeMonitor

newtype Grid n s = Grid (Map.Map n s) deriving (Show, Generic, Functor)

type GridS n = Grid n NodeS

gridS :: forall t m n . (IsStream t, MonadAsync m, Ord n) => [n] -> t m (n, NodeSensors s) -> t m (GridS n)
gridS ns = S.postscan gridMap
  where
    gridMap = Grid <$> FL.demux nodeMap
      where
        nodeMap = Map.fromList $ zip ns $ repeat nodeMonitor 

{----------------------------------------------------------

                CSV Conversion
----------------------------------------------------------}

newtype NamedNode n = NamedNode (n, NodeS) deriving (Generic)

instance (ToField n) => ToNamedRecord (NamedNode n) where
  toNamedRecord (NamedNode (n, ns)) = HM.fromList [("NodeId", toField n)] <> toNamedRecord ns

instance DefaultOrdered (NamedNode n) where
  headerOrder _ = Vec.fromList ["NodeId", "time"]
                  -- <> (headerOrder (undefined :: NodeSensors s))
                  <> headerOrder (undefined :: NodePower Watts)
                  <> headerOrder (undefined :: Energy WattSeconds)


{---------------------------------------------------------------------------------------------------------------------

                                          Helper Functions
---------------------------------------------------------------------------------------------------------------------}


storageSensors :: NodeSensors s -> SensorVector R
storageSensors es = SensorVector
  { sensorTerminalV = es ^. batteryVoltage
  , sensorCurrent =  i + o
  }
  where
    i = - (es ^. gridToBatteryCurrent + es ^. solarInputCurrent)
    o = es ^. batteryToGridCurrent + es ^. batteryToLoadCurrent

nodePower :: NodeSensors s -> NodePower Watts
nodePpower es = NodePower
           { tInP = txIn'
           , tOutP = txOut'
           , loadP = cnsm'
           , genP = genP' }
  where
    txIn' = p batteryVoltage gridToBatteryCurrent
    txOut' = p batteryVoltage batteryToGridCurrent
    cnsm' = p batteryVoltage batteryToLoadCurrent
    genP' = p batteryVoltage solarInputCurrent


-- $ converts the millisecond timestamp in the NodeSensors s to a UTCTime  
utcTimeNow :: NodeSensors s -> Time.UTCTime
utcTimeNow es = posixSecondsToUTCTime $ (fromIntegral $ (es ^. cpuTime))



