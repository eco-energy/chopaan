{-# LANGUAGE TypeApplications, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, DeriveAnyClass, DerivingStrategies, DerivingVia, DeriveGeneric, DeriveFunctor #-}
module Chopaan.Kibbutz.LinOpt where

import GHC.Generics
import Data.SBV
import Data.List
import qualified Algebra.Graph.Labelled as AG
import qualified Algebra.Graph as G
import Algebra.Graph.Label (Distance(..), Capacity(..), getDistance, getCapacity)
import qualified Numeric.Units.Dimensional.Prelude as D
--class 

class TP a n where
  getVals :: a n -> [Double]
  getNames :: a n -> [n]


type OptGraph a = AG.Graph (Distance a) (Capacity a)

newtype Cost a = Cost a
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Fractional, Real, RealFrac)

cost :: Num a => Distance a -> Capacity a -> Cost a
cost d g = Cost $ (getDistance d) * (getCapacity g) 

type CostGraph a = G.Graph (Cost a)

constrainDemand :: (Foldable f, Functor f, Num a) => f a -> a -> Goal 
constrainDemand nodeIncomings nodeDemand = constrain $ sum nodeIncomings .>= nodeDemand

constrainSupply :: (Foldable f, Functor f, Num a) => f a -> a -> Goal 
constrainSupply nodeOutgoings nodeSpareCapacity = constrain
                                                  $ sum nodeOutgoings .<= nodeSpareCapacity



txF :: OptGraph a -> Goal
txF g = undefined

transportProblem :: Num a => OptGraph a -> Goal
transportProblem g = do
  vars <- txVars
  mapM_ 
  mapM_ 
  minimize "goal" $ sum $ (fmap sum) $ hadmard vars (fmap (fmap fromDouble) cs)
  where
    txGraph :: AG.Graph (Distance a) (Capacity a) -> Symbolic (G.Graph SReal)
    txGraph = sequence . (fmap sequence) $ [[sReal $ tName i j
                                           |i <- getNames ss]
                                          | j <- getNames ds]
    fromDouble :: Double -> SReal
    fromDouble = realToFrac
    hadmard :: (Num a) => [[a]] -> [[a]] -> [[a]]
    hadmard as bs = fmap (\(xs, ys) -> fmap (\(x, y) -> x * y) $ zip xs ys) $ zip as bs
{-# INLINE transportProblem #-}


tName :: Show a => a -> a -> String
tName i j = ("x_" <> (show i) <> "_" <> (show j))
{-# INLINE tName #-}

fromName :: String -> (String, String)
fromName ('x':'_':next) = let
  f = takeWhile (\x -> x /= '_') next
  s = drop (length f + 1) next
  in (f, s)
fromName (_) = error "This Should ONLY Be Called for a tName"
{-# INLINE fromName #-}
