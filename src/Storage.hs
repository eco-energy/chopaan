{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ConstraintKinds #-}
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
  { gamma = 1
  , efficiency = 0.85
  , chargeCapacity = 4320000 -- In WattSeconds
  , ohmicResistance = 0.0035 -- In Ohms
  , diffusionResistance = 0.003  -- In Ohms
  , diffusionCapacitance = 50   -- In Farads
  , maxAbsAnalogHysteresisV = 1.2
  , instantaneousHysteresisV = 0.8
  }

-- $ Create Battery Parameters at a certain AmpH of capacity
initBP :: Double -> BatteryParams Double
initBP cc = defBatteryParams {chargeCapacity = ampHToWS cc}
  where
    ampHToWS a = (a * 12) * (60 * 60)


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

type ParamType a = (RealFrac a, Floating a, Ord a, Enum a)


soCtoOCV :: (ParamType a) => BatteryParams a -> a -> a
soCtoOCV BatteryParams{..} soc = intervals !! index
  where
    index = mod (round (soc / chargeCapacity)) 10
    intervals = [11.61, 11.76, 11.91, 12.06, 12.20, 12.34, 12.47, 12.60, 12.72, 12.83]

ocvToSoC :: forall a. (ParamType a) => BatteryParams a -> a -> a
ocvToSoC BatteryParams{..} v_t = socPercentage * chargeCapacity
  where
    socPercentage :: a
    socPercentage = roundDown v_t
    intervals = zip [0.0, 0.1 .. 1.0] [11.61, 11.76, 11.91, 12.06, 12.20, 12.34, 12.47, 12.60, 12.72, 12.83]
    roundDown v = goTillGreaterThan v intervals
    goTillGreaterThan _ [] = 1.0
    goTillGreaterThan v ((i, v'):xs) = if v > v' then (goTillGreaterThan v xs) else i


processModel :: forall a. (ParamType a)
  => BatteryParams a 
  -> a -- time since last process model update
  -> AugmentState StateVector SensorVector a -- prior (augmented) state
  -> AugmentState StateVector SensorVector a -- posterior (augmented) state
processModel ((bp@BatteryParams{..})) dt (AugmentState st@StateVector{..} sensor@SensorVector{..}) = AugmentState state' $ sensor'
  where
    state' = st
      { soC = z_next bp soC dt sensorCurrent
      , diffusionCurrent = i_rkn bp dt diffusionCurrent sensorCurrent
      , hysteresisVoltage = h_kn bp dt sensorCurrent hysteresisVoltage
      }
    sensor' = sensor {sensorTerminalV = predictedTerminalV bp state' sensor}
    -- SoC state equation

z_next :: Fractional a => BatteryParams a -> a -> a -> a -> a
z_next BatteryParams{chargeCapacity} z_prev delT i_k = z_prev - (delT / chargeCapacity) * (i_k)  -- + w_k  -- -> can add a noise parameter
  -- hysteresis voltage equation
h_kn :: (Ord a, Floating a) => BatteryParams a -> a -> a -> a -> a
h_kn BatteryParams{..} delT i_k h_kp = (expTerm * h_kp) + ((1 - expTerm) * (sgn i_k))
  where
    expTerm = exp (- (abs (delT * gamma * i_k * efficiency / chargeCapacity)))
    -- diffusion resistance current
i_rkn :: Floating a => BatteryParams a -> a -> a -> a -> a
i_rkn BatteryParams{..} delT i_rkp i_k =  (expTerm * i_rkp) + ((1 - expTerm) * i_k)
  where
    expTerm = exp ((- delT) / diffusionResistance * diffusionCapacitance)

-- ESC output equation computes voltage
-- v_k = OCV(z_k) + M (h_k) + M0 * s_k - sim (R_i i_rk - R0*ik)
-- M is the maximun absolute analog hysteresis voltage at the temperature
-- M0 is the instantaneous hysteresis voltage
-- R0 is the pure ohmic resistance

predictedTerminalV :: (ParamType a) => BatteryParams a -> StateVector a -> SensorVector a -> a
predictedTerminalV (bp@BatteryParams{..}) (StateVector{..}) (SensorVector{..}) = (ocv + hystCompV - curCompV)
  where
    ocv = soCtoOCV bp soC
    hystCompV = (maxAbsAnalogHysteresisV * hysteresisVoltage) + (instantaneousHysteresisV * sK)
    curCompV =  (diffusionResistance * diffusionCurrent) - (sensorCurrent * ohmicResistance)
    sK = sgn sensorCurrent -- if ((abs sensorCurrent) > 0) then  else sKp


sgn :: (Fractional a, Ord a) => a -> a
sgn a
  | a > 0 = 1
  | a < 0 = -1
  | otherwise = 0

initCov :: ParamType a => StateVector (StateVector a)
initCov = s
  where
    s = StateVector
          { soC = pure (1e-6)
          , diffusionCurrent = pure 1e-8
          , hysteresisVoltage = pure 2e-4
          }


processNoise :: ParamType a => StateVector a
processNoise = fmap (^ (2 :: Int)) $ pure (1e-1)

-- Normal distribution independent of timestep
sensorNoise :: ParamType a => SensorVector a
sensorNoise = pure 2e-1

initDynamic :: forall a. (ParamType a) => StateVector a
initDynamic = (pure (0 :: a))



{---------------------------------------- RUNNING THE KALMAN FILTER --------------------------------------}

type KalmanState m a = StateT (a, KalmanFilter StateVector a) m

type KF a = KalmanFilter StateVector a

initKF :: ParamType a => KF a
initKF = KalmanFilter (pure 0) initCov

runKalmanState :: (ParamType a) => a -> StateVector a -> KalmanState m a b -> m (b, (a, KalmanFilter StateVector a))
runKalmanState ts stateVec = (flip runStateT) (ts, (KalmanFilter stateVec initCov))


-- The current sensor bias is a part of the state equation
-- the voltage sensor bias is part of the output equation
runProcessModel :: (Monad m, ParamType a) => BatteryParams a -> a -> StateVector a -> SensorVector a -> SensorVector a -> KalmanState m a ()
runProcessModel battery dt noise senNoise sensorReadings = do
  (ts, prior) <- get
  let procPosterior@(KalmanFilter state' cov') = augmentProcess baseProcessModel extraState baseProcessUncertainty extraProcessUncertainty prior
  put (ts, procPosterior) -- KalmanFilter state' p' = (ts, KalmanFilter state' p')
  where
    baseProcessModel = EKFProcess $ processModel (auto <$> battery) (auto dt)
    extraState = sensorReadings
    baseProcessUncertainty = scaled noise
    extraProcessUncertainty = scaled senNoise


{--
runMeasurementModel :: (Monad m, ParamType a) => SensorVector a -> SensorVector a -> KalmanState m a (SensorVector (a, a))
runMeasurementModel noise measurement = sequence $ undefined

{-- measure ::
   SensorVector (Var t, t)
-> SensorVector (SensorVector (Var t))
-> Filter t (State t) (Var t)
-> (MeasureQuality t obs, Filter t (State t) (Var t))

--}

type VoltageMeasurement a = EKFMeasurement (SensorVector) (SensorVector (SensorVector a))


predV :: (ParamType a) => BatteryParams a -> StateVector a -> EKFMeasurement SensorVector a
predV bp st = measure (a, EKFMeasurement (predictedTerminalV (auto <$> bp))) senNoise
  where
    a = undefined

--mModel :: BatteryParams a -> _ -> KF a -> (KalmanInnovation (SensorVector) a, KF a)
--mModel (bp@BatteryParams{..}) obsCov (prior@(KalmanFilter st stCov)) = measure (predV bp) obsCov prior


--}
