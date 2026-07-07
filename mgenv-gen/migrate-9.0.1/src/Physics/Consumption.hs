{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Consumption spec + sampler (generation only). Runtime state / streamly
-- consumption stream dropped.
module Physics.Consumption
  ( ConsumptionSpec(..)
  , Load(..)
  , sampleConsumptionSpec
  , sampleLoadSpec
  ) where

import Physics.Units
import GHC.Generics (Generic)
import Control.Monad (replicateM, liftM)
import Prob.Randomizable

data Load = Load
  { power :: Watts
  , utility :: R
  } deriving (Eq, Ord, Show, Generic)

instance Randomizable Load where
  sampleThis = sampleLoadSpec

newtype ConsumptionSpec = ConsumptionSpec [Load] deriving (Eq, Ord, Show, Generic)

instance Randomizable ConsumptionSpec where
  sampleThis = sampleConsumptionSpec

sampleLoadSpec :: (MonadDistribution m) => m Load
sampleLoadSpec = do
  p <- uniformD [5, 10 .. 200]
  u <- liftM abs $ normal 0.5 0.2
  return $ Load p u

sampleConsumptionSpec :: MonadDistribution m => m ConsumptionSpec
sampleConsumptionSpec = do
  n <- uniformD [1 .. 10]
  loads <- replicateM n sampleLoadSpec
  return $ ConsumptionSpec loads
