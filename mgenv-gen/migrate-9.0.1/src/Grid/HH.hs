{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Household spec + sampler (generation only). Household *dynamics* (HHState
-- pipes, the ConCat HH GADT, streamly, Physics.Time) are dropped for 9.0.1.
module Grid.HH
  ( HHSpec(..)
  , NodeId
  , sampleHH
  ) where

import GHC.Generics (Generic)
import Physics.Units (GeoC, EuclideanC)
import Physics.Storage (BatterySpec, sampleBatterySpec)
import Physics.PV (PVSpec, samplePVSpec)
import Physics.Consumption (ConsumptionSpec, sampleConsumptionSpec)
import Prob.Randomizable

type NodeId = Int

data HHSpec = HHSpec
  { nId         :: NodeId
  , loc         :: GeoC
  , gridLoc     :: EuclideanC
  , storage     :: BatterySpec
  , generation  :: PVSpec
  , consumption :: ConsumptionSpec
  } deriving (Eq, Show, Generic)

instance Ord HHSpec where
  compare a b = compare (nId a) (nId b)

sampleHH :: (MonadDistribution m) => NodeId -> GeoC -> EuclideanC -> m HHSpec
sampleHH n loc' grloc = do
  storage' <- sampleBatterySpec
  gen      <- samplePVSpec
  consump  <- sampleConsumptionSpec
  return $ HHSpec n loc' grloc storage' gen consump
