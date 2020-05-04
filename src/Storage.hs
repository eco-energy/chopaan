{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Storage where

import Numeric.Estimator.KalmanFilter
import Numeric.Estimator.Augment
import Numeric.Estimator.Class ()
import Numeric.Estimator.Model.Symbolic ()

import GHC.Generics (Generic)

--import Control.Lens
--import Control.Applicative
import Data.Distributive
import Data.Foldable ()
import Data.Traversable ()
import Linear
import Control.Monad.State.Lazy 
import Numeric.AD
import Numeric.AD.Internal.Reverse ()


-- Two Goals
-- 1) SoC Estimation
-- 2) Battery Health Estimatio
-- 3) Power Limit Estimation


-- We use the ESC cell model to implement an EKF for the SoC

-- ESC state equation:
-- x_k+1 = A(i_k) * x_k + fn(i_k)
    

-- hysteresis voltage

data BatteryParams a = BatteryParams
  { gamma :: !a -- unitless constant γ adjusts how quickly the hysteresis state changes with a change in cell SOC
  , efficiency :: !a
  , chargeCapacity :: !a
  , ohmicResistance :: !a
  , diffusionResistance :: !a
  , diffusionCapacitance :: !a
  , maxAbsAnalogHysteresisV :: !a
  , instantaneousHysteresisV :: !a
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)


defBatteryParams :: BatteryParams Double
defBatteryParams = BatteryParams
  { gamma = 0
  , efficiency = 0.8
  , chargeCapacity = 3000
  , ohmicResistance = 10
  , diffusionResistance = 2
  , diffusionCapacitance = 2
  , maxAbsAnalogHysteresisV = 3
  , instantaneousHysteresisV = 3
  }

instance Applicative BatteryParams where
  pure v = BatteryParams
      { gamma = v
      , efficiency = v
      , chargeCapacity = v
      , ohmicResistance = v
      , diffusionResistance = v
      , diffusionCapacitance = v
      , maxAbsAnalogHysteresisV = v
      , instantaneousHysteresisV = v
      }
  v1 <*> v2 = BatteryParams
    { gamma = gamma v1 $ gamma v2
    , efficiency = efficiency v1 $ efficiency v2
    , chargeCapacity = chargeCapacity v1 $ chargeCapacity v2
    , ohmicResistance = ohmicResistance v1 $ ohmicResistance v2
    , diffusionResistance = diffusionResistance v1 $ diffusionResistance v2
    , diffusionCapacitance = diffusionCapacitance v1 $ diffusionCapacitance v2
    , maxAbsAnalogHysteresisV = maxAbsAnalogHysteresisV v1 $ maxAbsAnalogHysteresisV v2
    , instantaneousHysteresisV = instantaneousHysteresisV v1 $ instantaneousHysteresisV v2
    }

data StateVector a = StateVector
  { soC :: !a
  , diffusionCurrent :: !a
  , hysteresisVoltage :: !a
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

instance Applicative StateVector where
  pure v = StateVector
    { soC = v
    , diffusionCurrent = v
    , hysteresisVoltage = v
    }
  v1 <*> v2 = StateVector
              { soC = soC v1 $ soC v2
              , diffusionCurrent = diffusionCurrent v1 $ diffusionCurrent v2
              , hysteresisVoltage = hysteresisVoltage v1 $ hysteresisVoltage v2
              }

instance Distributive StateVector where
  distribute f = StateVector 
    { soC = fmap soC f
    , diffusionCurrent = fmap diffusionCurrent f
    , hysteresisVoltage = fmap hysteresisVoltage f
    }


data SensorVector a = SensorVector
  { sensorTerminalV :: !a
  , sensorCurrent :: !a
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)


instance Applicative SensorVector where
  pure v = SensorVector v v
  v1 <*> v2 = SensorVector
    { sensorTerminalV = sensorTerminalV v1 $ sensorTerminalV v2
    , sensorCurrent = sensorCurrent v1 $ sensorCurrent v2
    }


instance Distributive SensorVector where
  distribute f = SensorVector
    { sensorTerminalV = fmap sensorTerminalV f
    , sensorCurrent = fmap sensorCurrent f
    }



processModel :: forall a. (Fractional a, Floating a, Ord a)
  => BatteryParams a 
  -> a -- time since last process model update
  -> AugmentState StateVector SensorVector a -- prior (augmented) state
  -> AugmentState StateVector SensorVector a -- posterior (augmented) state
processModel ((BatteryParams{..})) dt (AugmentState st@StateVector{..} SensorVector{..}) = AugmentState state' $ sensor'
  where
    state' = st
      { soC = z_next soC dt sensorCurrent
      , diffusionCurrent = i_rkn dt diffusionCurrent sensorCurrent
      , hysteresisVoltage = h_kn dt sensorCurrent hysteresisVoltage
      }
    sensor' = (pure 0)
    -- SoC state equation
    z_next z_prev delT i_k = z_prev - (delT / chargeCapacity) * (i_k) --  + w_k -> can add a noise parameter
  -- hysteresis voltage equation
    h_kn delT i_k h_kp = (expTerm * h_kp) + ((1 - expTerm) * (sgn i_k))
      where
        expTerm = exp (- (abs (delT * gamma * i_k * efficiency / chargeCapacity)))
    -- diffusion resistance current
    i_rkn delT i_rkp i_k =  (expTerm * i_rkp) + ((1 - expTerm) * i_k)
      where
        expTerm = exp ((- delT) / diffusionResistance * diffusionCapacitance)

-- ESC output equation computes voltage
-- v_k = OCV(z_k) + M (h_k) + M0 * s_k - sim (R_i i_rk - R0*ik)
-- M is the maximun absolute analog hysteresis voltage at the temperature
-- M0 is the instantaneous hysteresis voltage
-- R0 is the pure ohmic resistance

predictedTerminalV :: (Floating a, Ord a, Monad m) => BatteryParams a -> StateVector a -> SensorVector a -> StateT a m a
predictedTerminalV (BatteryParams{..}) (StateVector{..}) (SensorVector{..}) = do
  sKp <- get
  let
    ocv = soCtoOCV soC
      where
        soCtoOCV = id
    hystCompV = (maxAbsAnalogHysteresisV * hysteresisVoltage) + (instantaneousHysteresisV * sK)
    curCompV =  (diffusionResistance * diffusionCurrent) - (sensorCurrent * ohmicResistance)
    sK = if ((abs sensorCurrent) > 0) then sgn sensorCurrent else sKp
  put $ sK
  return $ ocv + hystCompV - curCompV


sgn :: (Fractional a, Ord a) => a -> a
sgn a
  | a > 0 = 1
  | a < 0 = -1
  | otherwise = 0

initCov :: Fractional a => StateVector (StateVector a)
initCov = s
  where
    s = StateVector
          { soC = pure (1e-6)
          , diffusionCurrent = pure 1e-8
          , hysteresisVoltage = pure 2e-4
          }


processNoise :: Fractional a => StateVector a
processNoise = fmap (^ (2 :: Int)) $ pure (1e-1)

-- Normal distribution independent of timestep
sensorNoise :: Fractional a => SensorVector a
sensorNoise = pure 2e-1

initDynamic :: forall a. (Floating a) => StateVector a
initDynamic = (pure (0 :: a))



{---------------------------------------- RUNNING THE KALMAN FILTER --------------------------------------}

type KalmanState m a = StateT (a, KalmanFilter StateVector a) m

type KF a = KalmanFilter StateVector a

initKF :: Fractional a => KF a
initKF = KalmanFilter (pure 0) initCov

runKalmanState :: (Fractional a) => a -> StateVector a -> KalmanState m a b -> m (b, (a, KalmanFilter StateVector a))
runKalmanState ts stateVec = (flip runStateT) (ts, (KalmanFilter stateVec initCov))


-- The current sensor bias is a part of the state equation
-- the voltage sensor bias is part of the output equation
runProcessModel :: (Monad m, Floating a, Ord a) => BatteryParams a -> a -> StateVector a -> SensorVector a -> SensorVector a -> KalmanState m a ()
runProcessModel battery dt noise senNoise sensorReadings = do
  (ts, prior) <- get
  let posterior = augmentProcess baseProcessModel extraState baseProcessUncertainty extraProcessUncertainty prior
  put (ts, posterior) -- KalmanFilter state' p' = (ts, KalmanFilter state' p')
  where
    baseProcessModel = EKFProcess $ processModel (auto <$> battery) (auto dt)
    extraState = sensorReadings
    baseProcessUncertainty = scaled noise
    extraProcessUncertainty = scaled senNoise

