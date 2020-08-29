module SimNode where

import Lens.Micro

import Control.Monad.Bayes.Class

import Chopaan.Node.NodeId
import Chopaan.Comm.Comm
import Chopaan.Comm.Mqtt (runMqtt)
import Chopaan.Types (MQTTOpts)
import Proto.NodeMessageSchema.NodeMessages

import Control.Monad.Trans.State
import Data.Time
import Data.Time.Clock (addUTCTime, NominalDiffTime)
import Data.Time.LocalTime.Compat (TimeOfDay, LocalTime, addLocalTime, diffLocalTime, localTimeToUTC)

nodeDist :: (MonadSample m) => StateT (LocalTime, EnergyState) m EnergyState
nodeDist = do
  (t, oldState) <- get
  batteryVoltageDiff <- normal 12 1
  loadCurrentDiff <- abs <$> normal 30 20
  gridCurrentDiff <- normal 0 20
  solarCurrentDiff <- abs <$> normal 10 10
  solarVoltageDiff <- case (daytime t) of
    True -> normal 17 5
    False -> normal 0.3 0.4
  let
    t' = addLocalTime (1 :: NominalDiffTime) t
    newState = oldState
  put $ (t', newState)
  return undefined
  where
    daytime :: LocalTime -> Bool
    daytime t = t > rise && t < set
    (rise, set) = (startDay $ TimeOfDay 6 0 0, startDay $ TimeOfDay 18 0 0)
    startDay = LocalTime $ fromGregorian 1 1 2020

runtimeDist :: (MonadSample m) => m RuntimeStats
runtimeDist = undefined

logsDist :: (MonadSample m) => m MeshFrame
logsDist = undefined



solarDay :: [(Double, Double)]
solarDay = [(10, 10)]

temporalGaussians :: (MonadSample m) => (LocalTime, LocalTime) -> [(Double, Double)] -> (LocalTime -> m Double)
temporalGaussians (_, _) [] _ = return 0
temporalGaussians (start, end) ranges@(r:rs) t = do
  let
    sections = length ranges
    sectionLength = diffLocalTime end start / (fromIntegral $ sections)
    t' = addLocalTime sectionLength start
  case isBetween start t' t of
    True -> (uncurry normal) r
    False -> temporalGaussians (t', end) rs t
  where
    isBetween s e x = x >= s && x <= e  
