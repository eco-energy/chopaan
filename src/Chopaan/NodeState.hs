{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module Chopaan.NodeState where

import qualified Prelude as P

import qualified Proto.NodeMessageSchema.NodeMessages as NM
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as NM

import GHC.Generics hiding (C, R)

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.SIUnits
import Numeric.Units.Dimensional.Quantities
import Numeric.Units.Dimensional

import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Lens.Micro


type R = Double
type C s = ElectricCurrent s
type V s = ElectricPotential s


--type Power s = ElectricCurrent s * ElectricPotential s

vi :: (Num s) => (s, s) -> Power s
vi = uncurry (toP @R)

toP :: forall s. (Num s) => s -> s -> Power s
toP i v = toI i * toV v


toV :: (Num s) => s -> V s
toV v = (v *~ volt)

toI :: (Num s) => s -> C s
toI v = (v *~ ampere)

--type A s = (ElectricPotential s) * (ElectricCurrent s)


data NodeState s = NodeState
  { batteryVoltage :: V s
  , gridVoltage :: V s
  , loadCurrent :: C s
  , gridCurrent :: C s
  , solarCurrent :: C s
  , temperature :: s
  , nodeTime :: UTCTime
  } deriving (Eq, Ord, Show, Generic)



fromNodeMessage :: NM.EnergyState -> NodeState R
fromNodeMessage nm = NodeState
                     { batteryVoltage = toV $ nm ^. NM.batteryVoltage
                     , gridVoltage = toV $ nm ^.  NM.gridVoltage
                     , loadCurrent = toI $ nm ^.  NM.batteryToLoadCurrent
                     , gridCurrent = cOut + cIn
                     , solarCurrent = toI $ nm ^. NM.solarInputCurrent
                     , temperature = nm ^. NM.temperature
                     , nodeTime = (nodeTimeToUTC nm)
                     }
  where
    cOut :: C R
    cOut = toI $ nm ^.  NM.batteryToGridCurrent
    cIn :: C R
    cIn = toI $ (-1) P.* (nm ^. NM.gridToBatteryCurrent)


-- $ converts the millisecond timestamp in the EnergyState to a UTCTime  
nodeTimeToUTC :: NM.EnergyState -> UTCTime
nodeTimeToUTC es = posixSecondsToUTCTime $ (fromIntegral $ (es ^. NM.cpuTime))
