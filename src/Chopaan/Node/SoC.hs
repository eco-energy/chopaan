{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
module Chopaan.SoC where

{--
import GHC.Generics (Generic)
--import Numeric.LinearAlgebra.Static hiding ((<>))
--import qualified Numeric.LinearAlgebra as LA
--import qualified Numeric.LinearAlgebra.Static as LA


import Streamly
import qualified Streamly.Prelude as S

import GHC.TypeLits
import qualified Data.Time as Time

import Control.Monad.State (runStateT, StateT, get, put)
import Numeric.Estimator


type KalmanState m a = StateT (a, KalmanFilter StateVector a) m

runKalmanState :: (Monad m, Fractional a) => a -> StateVector a -> KalmanState m a b -> m (b, (a, KalmanFilter StateVector a))
runKalmanState ts state = runStateT (ts, KalmanFilter state undefined)


type Step = Time.DiffTime

--newtype VI i n = VI { unVI :: (KnownNat n, Ord i) => (i, L n 2) } -- deriving (Eq, Ord, Show)

newtype VI a = VI { unVI :: (a, a) }

type VI' = VI ℝ

mkVI :: (Num a) => a -> a -> VI a   
mkVI = curry VI

mkVI' :: ℝ -> ℝ -> VI'
mkVI' = mkVI



data BatteryState a = BatteryState
  { vi :: VI a
  , diffTime :: Time.DiffTime
  }


states :: (KnownNat n) => i -> t m VI' -> t m (L n 2)
states = undefined



{--
deltaT, r1, c1, cap, n' :: Double
deltaT = 0.01
r1 = 10.0
c1 = 5.0
cap = 10.0
n' = 0.8

qc :: Double
qc = 0.01

bigQl :: [Double]
bigQl = [ qc * deltaT^3 / 3, qc * deltaT^2 / 2,
          qc * deltaT^2 / 2,      qc * deltaT
        ]

bigQ :: Sym 2
bigQ = sym $ matrix bigQl

bigR :: Sym 1
bigR = sym $ matrix [0.1]

stateUpdate :: R 3 -> R 2
stateUpdate u = (a #> prev) + (b * duplicatedInput)
  where
    a :: L 2 2
    a = matrix [1.0, 0.0, 0.0, expt ]
    b :: R 2
    b = vector [(- n' * deltaT / cap), (1 - expt)]
    expt = exp (- deltaT / r1 * c1)
    duplicatedInput = vector [ik, ik]
    (ik, prev) = headTail u
    
    

observe :: R 3 -> R 1
observe a = vector [sin x] where x = fst $ headTail a

linearizedObserve :: R 3 -> L 1 3
linearizedObserve a = matrix [cos x, 0.0, 0.0] where x = fst $ headTail a

linearizedStateUpdate :: R 2 -> Sq 2
linearizedStateUpdate u = matrix [ 1.0,                  deltaT,
                                 (cos x1) * deltaT,   1.0]
                        where
                          (x1, _) = headTail u

singleEKF :: (R 2, Sym 2) -> R 1 -> (R 2, Sym 2)
singleEKF = runEKF (const observe) (const linearizedObserve) (const bigR)
                   (const stateUpdate) (const linearizedStateUpdate) (const bigQ)
                   undefined


singleUKF :: (R 2, Sym 2) -> R 1 -> (R 2, Sym 2)
singleUKF = runUKF (const observe) (const bigR) (const stateUpdate) (const bigQ)
                   undefined

initialDist :: (R 2, Sym 2)
initialDist = (vector [1.6, 0.0],
               sym $ matrix [0.1, 0.0,
                             0.0, 0.1])

multiEKF :: [ℝ] -> [(R 2, Sym 2)]
multiEKF obs = scanl singleEKF initialDist (map (vector . pure) obs)

multiUKF :: (IsStream t, Monad m) => t m ℝ -> t m (R 2, Sym 2)
multiUKF obs = S.scanl' singleUKF initialDist (S.map (vector . pure) obs)
--}





data EKFState a = EKFState
  { iR :: a
  , h :: a
  , soc :: a
  } deriving (Eq, Ord, Show, Generic, Functor)


data EKFModelParams a = EKFModelParams
  { q' :: a
  , g' :: a
  , m' :: a
  , m'' :: a
  , rc' :: a
  , r :: a
  , r' :: a
  , eta :: a
  } deriving (Eq, Ord, Show, Generic, Functor)


data Measurements a = Measurements
  { vT :: a
  , iT :: a
  , t :: Int
  } deriving (Eq, Ord, Show, Generic, Functor)


data EKFData a = EKFData
  { sigmaX :: a
  , sigmaY :: a
  , sigmaZ :: a
  , xhat :: a
  , priorI :: a
  } deriving (Eq, Ord, Show, Generic, Functor)

rc :: Floating a => a -> EKFModelParams a -> a
rc delT EKFModelParams {..} = exp (- delT / rc' )

xHat0 :: (Num a) => a -> EKFState a
xHat0 v = EKFState 0 0 v

initCovariance :: (Floating a) => EKFState a
initCovariance = EKFState 0.1 0.1 0.1  

data SoCParams a = SoCParams
  { ceff :: a
  , q :: a  -- estimated capacity
  , zp :: a
  } deriving (Eq, Ord, Show, Generic, Functor)

instance Applicative SoCParams where
  pure a = SoCParams a a a
  a <*> b = SoCParams
    { ceff = ceff a $ ceff b
    , q = q a $ q b
    , zp = zp a $ zp b
    }


data HystParams a = HystParams
  { ceffH :: a
  , iH :: a
  , delT :: a
  , rH:: a
  , cH :: a
  } deriving (Eq, Ord, Show, Generic)

data StateVector a = StateVector
  { stateOfCharge :: !(SoCParams a)
  , hysteresis :: (HystParams a)
  , resistance :: a
  , diffCurrent :: a
  } deriving (Eq, Ord, Show, Generic)

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
    
--}
