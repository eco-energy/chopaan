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
import Numeric.Estimator.Class
import Numeric.Estimator.Model.Symbolic

import GHC.Generics (Generic)

--import Control.Lens
--import Control.Applicative
import Data.Distributive
import Data.Foldable
import Data.Traversable
import Linear
import qualified Control.Monad.State.Lazy as S 
import Numeric.AD
import Numeric.AD.Internal.Reverse
-- Two Goals
-- 1) SoC Estimation
-- 2) Battery Health Estimatio
-- 3) Power Limit Estimation


-- We use the ESC cell model to implement an EKF for the SoC

-- ESC state equation:
-- x_k+1 = A(i_k) * x_k + fn(i_k)
-- ESC output equation computes voltage


-- v_k = OCV(z_k) + M (h_k) + M0 * s_k - sim (R_i i_rk - R0*ik)

-- M is the maximun absolute analog hysteresis voltage at the temperature
-- M0 is the instantaneous hysteresis voltage
-- R0 is the pure ohmic resistance

-- hysteresis voltage

data BatteryParams a = BatteryParams
  { gamma :: !a
  , efficiency :: !a
  , chargeCapacity :: !a
  , ohmicResistance :: !a
  , diffusionResistance :: !a
  , diffusionCapacitance :: !a
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

-- sensorvector i_k
-- stateVector hysteresisVoltage, diffusionCurrent, soC


-- The current sensor bias is a part of the state equation
-- the voltage sensor bias is part of the output equation


data StateVector a = StateVector
  { stateSoC :: !a
  , diffusionCurrent :: !a
  , hysteresisVoltage :: !a
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

instance Applicative StateVector where
  pure v = StateVector
    { stateSoC = v
    , diffusionCurrent = v
    , hysteresisVoltage = v
    }
  v1 <*> v2 = StateVector
              { stateSoC = stateSoC v1 $ stateSoC v2
              , diffusionCurrent = diffusionCurrent v1 $ diffusionCurrent v2
              , hysteresisVoltage = hysteresisVoltage v1 $ hysteresisVoltage v2
              }

instance Distributive StateVector where
  distribute f = StateVector 
    { stateSoC = fmap stateSoC f
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
  => BatteryParams a -- time since last process model update
  -> a
  -> AugmentState StateVector SensorVector a -- prior (augmented) state
  -> AugmentState StateVector SensorVector a -- posterior (augmented) state
processModel (b@(BatteryParams{..})) dt (AugmentState state@StateVector{..} sensor@SensorVector{..}) = AugmentState state' $ sensor'
  where
    state' = state
      { stateSoC = z_next stateSoC dt sensorCurrent 0
      , diffusionCurrent = i_rkn dt diffusionCurrent sensorCurrent
      , hysteresisVoltage = h_kn dt sensorCurrent hysteresisVoltage
      }
    sensor' = (pure 0)
    -- SoC state equation
    z_next z_prev delT i_k w_k = z_prev - (delT / chargeCapacity) * (i_k + w_k)
  -- hysteresis voltage equation
    h_kn delT i_k h_kp = (expTerm * h_kp) + ((1 - expTerm) * (sgn i_k))
      where
        expTerm = exp (- (abs (delT * gamma * i_k * efficiency / chargeCapacity)))
        sgn a
          | a > 0 = 1
          | a < 0 = -1
          | otherwise = 0
    -- diffusion resistance current
    i_rkn delT i_rkp i_k =  expDiff * i_rkp + ((1 - expDiff) * i_k)
      where
        expDiff = exp ((- delT) / diffusionResistance * diffusionCapacitance)


initCov :: Fractional a => StateVector (StateVector a)
initCov = s
  where
    s = StateVector
          { stateSoC = pure (1e-6)
          , diffusionCurrent = pure 1e-8
          , hysteresisVoltage = pure 2e-4
          }


initDynamic :: forall a. (Floating a) => a -> a -> a -> StateVector a
initDynamic soc volt cur = (pure (0 :: a))
  { stateSoC = soc
  , diffusionCurrent = volt
  , hysteresisVoltage = cur
  }



{---------------------------------------- RUNNING THE KALMAN FILTER --------------------------------------}

type KalmanState m a = S.StateT (a, KalmanFilter StateVector a) m

runKalmanState :: (Fractional a) => a -> StateVector a -> KalmanState m a b -> m (b, (a, KalmanFilter StateVector a))
runKalmanState ts stateVec = (flip S.runStateT) (ts, (KalmanFilter stateVec initCov))

runProcessModel :: (Monad m, Floating a, Ord a, Mode a) => BatteryParams a -> a -> StateVector a -> SensorVector a -> SensorVector a -> KalmanState m a ()
runProcessModel battery dt noise viNoise vi = do
  (ts, prior) <- S.get
  let out = augmentProcess baseProcessModel extraState baseProcessUncertainty extraProcessUncertainty prior
  S.put (ts, out) -- KalmanFilter state' p' = (ts, KalmanFilter state' p')
  where
    baseProcessModel = EKFProcess $ processModel (auto <$> battery) (auto dt)
    extraState = vi
    baseProcessUncertainty = scaled noise
    extraProcessUncertainty = scaled viNoise


-- EKFProcess :: (forall s. Reifies s Tape) => state (Reverse s var) -> state (Reverse s var) -> EKFProcess state var
