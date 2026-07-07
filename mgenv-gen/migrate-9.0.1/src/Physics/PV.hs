{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

-- | PV spec + sampler (generation only). The PVWatts solar model (runPV, astro
-- sun position, streamly) is dropped for the 9.0.1 generation library.
module Physics.PV
  ( Mount(..)
  , ModuleType(..)
  , PVSpec(..)
  , samplePVSpec
  ) where

import GHC.Generics (Generic)
import Physics.Units (R, Watts)
import Control.Monad (liftM)
import Prob.Randomizable

data Mount = OpenRack | CloseRoofMount | InsulatedBack | Tracker
  deriving (Eq, Ord, Show, Generic)

data ModuleType = GlassCellGlass | GlassCellPolymerSheet | PolymerThinFilmSteel
  deriving (Eq, Ord, Show, Generic)

data PVSpec = PVSpec
  { arrAzimuth     :: !R
  , arrTilt        :: !R
  , tempCorrection :: !R
  , power          :: !Watts
  , mount          :: !Mount
  , moduleType     :: !ModuleType
  } deriving (Eq, Ord, Show, Generic)

instance Randomizable PVSpec where
  sampleThis = samplePVSpec

samplePVSpec :: (MonadDistribution m) => m PVSpec
samplePVSpec = do
  let arrAz = 180                              -- assume southward facing
  arrTilt <- normal 30 10
  tempCorrection <- liftM abs $ normal 0.0044 0.05
  power <- uniformD [50, 100 .. 500]
  return $ PVSpec arrAz arrTilt tempCorrection power OpenRack GlassCellGlass
