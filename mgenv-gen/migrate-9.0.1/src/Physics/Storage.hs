{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Battery spec + sampler (generation only). The equivalent-circuit battery
-- dynamics (dimensional units, streamly) are dropped for 9.0.1 generation.
module Physics.Storage
  ( BatterySpec(..)
  , batterySpec
  , sampleBatterySpec
  ) where

import GHC.Generics (Generic)
import Physics.Units (Eff, AmpH, V, Amp)
import Control.Monad (liftM)
import Prob.Randomizable

data BatterySpec = BatterySpec
  { coloumbicEff        :: Eff
  , totalChargeCapacity :: AmpH
  , qMin                :: AmpH
  , qMax                :: AmpH
  , vNominal            :: V
  , vMin                :: V
  , vMax                :: V
  , delVDisAtI          :: V
  , iDis                :: Amp
  , delVChgAtI          :: V
  , iChg                :: Amp
  } deriving (Eq, Show, Generic)

instance Randomizable BatterySpec where
  sampleThis = sampleBatterySpec

batterySpec :: Eff -> AmpH -> AmpH -> AmpH -> V -> V -> V -> V -> Amp -> V -> Amp -> BatterySpec
batterySpec = BatterySpec

sampleBatterySpec :: MonadDistribution m => m BatterySpec
sampleBatterySpec = do
  eff <- do
      c <- normal 0.8 0.2
      d <- normal 0.9 0.2
      return (c, d)
  cap  <- uniformD [40, 50 .. 400]
  qMin <- liftM (* cap) $ uniform 0.2 0.5
  qMax <- liftM (* cap) $ uniform 0.8 0.99
  vNom <- normal 12 0.5
  vMax <- liftM ((+) vNom . abs) $ normal 2.0 1.0
  vMin <- liftM ((-) vNom . abs) $ normal 2.0 1.0
  dischargeDeltaV  <- uniform 0.1 0.5
  dischargeRefCurr <- uniform 0.1 40
  chargeDeltaV     <- uniform 0.1 0.5
  chargeRefCurr    <- uniform 0.1 40
  return $ batterySpec eff cap qMin qMax vNom vMin vMax dischargeDeltaV dischargeRefCurr chargeDeltaV chargeRefCurr
