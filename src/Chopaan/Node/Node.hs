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
module Chopaan.Node.Node (
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

import Data.Time (UTCTime)
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

import Chopaan.Node.Storage
import Chopaan.Node.NodeSensors

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




secsToMinutes :: (Num a) => a -> a
secsToMinutes = (* 60)

nmFilter :: (NodeId a) -> NodeS -> Bool
nmFilter _ = isJust . _time





{----------------------------------------------------------------------------------------------------


Folds of type FL.Fold, as functions to the instantatenous values of the system over an indexed set.
                      :: forall s. Fold (s -> a -> m s) (m s) (s -> m b)


-----------------------------------------------------------------------------------------------------}

-- Converts Unit s to Compensated (Unit s)
-- calculates the time difference multiplier
integrationFold :: forall m s. (Monad m, Num s) => FL.Fold m (NodeSensors s) (Energy s)
integrationFold = undefined

timeFold :: forall m. Monad m => FL.Fold m NodeSensors s UTCTime
timeFold = FL.Fold step' begin' done'
  where
    step' :: (UTCTime -> NodeSensors s -> m UTCTime)
    step' (Nothing, _) cur = pure (Just tn, diffUTC tn tn)
      where
        tn = utcTimeNow cur
    step' (Just !prev, _) cur = pure (Just tn, diffUTC tn prev)
      where
        tn = utcTimeNow cur
    begin' :: m UTCTime
    begin' = pure (Nothing, 0)
    done' :: UTCTime -> m UTCTime
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
    eAtT :: NodePower Watts -> UTCTime -> Energy WattSeconds
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
    tn :: FL.Fold m NodeSensors s UTCTime
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

timeS :: (MonadAsync m, IsStream t) => t m NodeSensors s -> t m UTCTime
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





