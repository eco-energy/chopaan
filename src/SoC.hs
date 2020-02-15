{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
module SoC where


import GHC.Generics (Generic)
import Numeric.LinearAlgebra.Static
import Numeric.Kalman

import Streamly
import qualified Streamly.Prelude as S

deltaT, g :: Double
deltaT = 0.01
g = 9.81

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

stateUpdate :: R 2 -> R 2
stateUpdate u = vector [x1 + x2 * deltaT, x2 - g * (sin x1) * deltaT]
  where
    (x1, w) = headTail u
    (x2, _) = headTail w

observe :: R 2 -> R 1
observe a = vector [sin x] where x = fst $ headTail a

linearizedObserve :: R 2 -> L 1 2
linearizedObserve a = matrix [cos x, 0.0] where x = fst $ headTail a

linearizedStateUpdate :: R 2 -> Sq 2
linearizedStateUpdate u = matrix [ 1.0,                  deltaT,
                                -g * (cos x1) * deltaT,   1.0]
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
