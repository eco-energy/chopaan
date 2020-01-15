module SoC where

import Control.Applicative
import Data.Reflection
import Data.Traversable
import Numeric.AD
import Numeric.AD.Internal.Reverse
import Numeric.Estimator

import GHC.Generics (Generic)
import Data.Data

import Proto.NodeMessages
import Proto.NodeMessages_Fields

import Control.Monad.State (runStateT, StateT, get, put)


f v c temp t = do
  delT <- get
  iterEKF v i temp delT
  put (delT + t)

data EKFState = EKFState
  { iR :: Double , h :: Double, soc :: Double} deriving (Eq, Ord, Show, Generic)

data EKFModelParams a = EKFModelParams
  { q' :: a, g' :: a, m' :: a, m'' :: a, rc' :: a, r :: a,  r' :: a, eta :: a} deriving (Eq, Ord, Show, Generic)

data Measurements a = Measurements a
  { vT :: a, iT :: a, t :: Int } deriving (Eq, Ord, Show)

data EKFData a = EKFData a
  { sigmaX :: a
  , sigmaY :: a
  , sigmaZ :: a
  -- these should be what the scan is over. scanl' \(priorI, xhat) -> whatever
  , xhat :: a
  , priorI :: a
  } deriving (Eq, Ord, Show, Generic)

rc delT EKFModelParams {..} = exp (- delT / rc' )

xHat0 :: EKFState
xHat0 v = EKFState 0 0 v

initCovariance = EKFState 0.1 0.1 0.1  

-- 6 steps


iterEKF = fold xHat0



-- these are regressed using the following equations:
--    :*: ceff

class Regressionable a where
  train :: f x
        -- ^ sample 
        -> (x -> p a -> y)
        -- ^ parameter space
        -> (y -> y -> a)
        -- ^ loss function
        -> (p a, f y)
        -- ^ return type: (updated parameters, predictions)

data SoCParams a = SoCParams
  { ceff :: a
  , q :: a  -- estimated capacity
  , zp :: a
  } deriving (Eq, Ord, Show, Generic)


data HystParams a = HystParams
  { ceffH :: a, iH :: a, delT :: a,  rH:: a, cH :: a
  } deriving (Eq, Ord, Show, Generic)

data StateVector a = StateVector
  { stateOfCharge :: !(SoCParams a), hysteresis :: a, resistance :: a, diffCurrent :: a} deriving (Eq, Ord, Show, Generic)

type KalmanState m a = StateT (a, KalmanFilter StateVector a) m

runKalmanState :: (Monad m, Fractional a) => a -> StateVector a -> KalmanState m a b -> m (b, (a, KalmanFilter StateVector a))
runKalmanState ts state = runStateT (ts, KalmanFilter state initCovariance)


runProcessModel :: (Monad m, Floating a, Ord a) => a -> StateVector a -> a
runProcessModel = undefined


soc' :: (Fractional a) => a -> a -> a -> a -> a -> a
soc' zp ceff i delT q = zn
  where
    zn = zp - ((ceff * i * delT) / q)

diffCurrent' :: (Floating a) => a -> a -> a -> a -> a
diffCurrent' r c i delT = (r' * i) + ((1 - r') * i)
  where
    r' = exp ((- delT) / (r*c))

hyst' :: (Floating a) => a -> a -> a -> a -> a -> a -> a
hyst' hp ceff i delT gamma q = h' * hp + (1 - h') * hp
  where
    h' = exp (- abs ((ceff * i * gamma * delT) / q))

-- * Process Model Equations

-- | the energy estimation process model, driven by current and voltage measurements.
processModel :: Fractional a
             => a
             -- ^ time since last process model update
             -> AugmentState StateVector SoCParams a
             -- ^ prior (augmented) state
             -> AugmentState StateVector SoCParams a
             -- ^ posterior (augmented) state
processModel dt (AugmentState state p) = AugmentState state' $ pure 0
  where
    state' = undefined
