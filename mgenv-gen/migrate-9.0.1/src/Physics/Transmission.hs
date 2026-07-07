{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Transmission spec + sampler (generation only). The transmission *dynamics*
-- (streamly Pipes, ConCat.Isomorphism power-flow) are dropped for the 9.0.1
-- generation library.
module Physics.Transmission
  ( TransmissionSpec(..)
  , resistance
  , sampleTransmissionSpec
  ) where

import Physics.Units
import GHC.Generics (Generic)
import Prob.Randomizable

data TransmissionSpec = TransmissionSpec
  { wireLength :: Meters
  , crossSection :: MetersSq
  , resistivity :: OhmMeters
  } deriving (Eq, Show, Ord, Generic)

instance Semigroup TransmissionSpec where
  (<>) ts1 ts2 = TransmissionSpec wl' cs' rvity'
    where
      wl' = l1 + l2
      cs' = weightedSum cs1 cs2
      rvity' = weightedSum rvity1 rvity2
      weightedSum a b = scaleBy l1 a + scaleBy l2 b
        where scaleBy l c = c * (l / l1 + l2)
      TransmissionSpec {wireLength=l1, crossSection=cs1, resistivity=rvity1} = ts1
      TransmissionSpec {wireLength=l2, crossSection=cs2, resistivity=rvity2} = ts2

instance Monoid TransmissionSpec where
  mempty = TransmissionSpec 0 0 0

instance Randomizable TransmissionSpec where
  sampleThis = sampleTransmissionSpec

resistance :: TransmissionSpec -> Ohm
resistance TransmissionSpec{..} = wireLength * resistivity / crossSection

sampleTransmissionSpec :: (MonadDistribution m) => m TransmissionSpec
sampleTransmissionSpec = do
  wireLength <- uniform 10 100
  diameter <- uniformD [i / 1000 | i <- [0.75 .. 10]]
  resistivity <- normal 1.724e-8 ((1.724e-8 * 2) / 100)
  return $ TransmissionSpec wireLength (crossSection diameter) resistivity
  where
    crossSection d = pi * (d / 2) ** 2
