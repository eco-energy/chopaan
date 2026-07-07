{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module Prob.Randomizable
  ( Randomizable(..)
  , module Control.Monad.Bayes.Class) where

import Control.Monad.Bayes.Class
import GHC.Generics hiding (R)


class Randomizable a where
  sampleThis :: (MonadDistribution m) => m a
  -- DistParams a -> 
  --defaultDistParams :: DistParams a


type R = Double

data DistParams a = Uniform { lowerBound :: R, upperBound :: R}
            | Normal { mean :: R, stdDev :: R }
            | Gamma { shape :: R, scale :: R}
            | Beta  { alpha :: R, beta :: R }
            | Bernoulli { prob :: R}
            | Categorical { observables :: [R]}
            | UniformD { outcomes :: [a] }
            | Geometric { successRate :: R }
            | Poisson { lambda :: R }
            | Dirichlet { concentration :: [R]}
            | Compound [DistParams a]
            deriving (Eq, Ord, Show, Generic)


mkUniform :: R -> R -> DistParams a
mkUniform l u
  | l >= u = Uniform l u
  | otherwise = Uniform u l


mkNormal :: R -> R -> DistParams a
mkNormal = Normal


mkGamma :: R -> R -> DistParams a
mkGamma = Gamma

mkBeta :: R -> R -> DistParams a
mkBeta = Beta

mkBernoulli :: R -> DistParams a
mkBernoulli = Bernoulli

mkCategorical :: [R] -> Maybe (DistParams a)
mkCategorical xs
  | sum xs < 0 || sum xs > 1 = Nothing
  | otherwise = Just $ Categorical xs

mkUniformD :: [a] -> (DistParams a)
mkUniformD = UniformD

mkGeometric :: R -> DistParams a
mkGeometric = Geometric

mkPoisson :: R -> DistParams a
mkPoisson = Poisson

mkDirichlet :: [R] -> DistParams a
mkDirichlet = Dirichlet

{--
sampleFromDist :: (MonadDistribution m) => DistParams a -> (R -> a) -> m a
sampleFromDist Uniform{lowerBound, upperBound} = do
  uniform lowerBound upperBound
--}
