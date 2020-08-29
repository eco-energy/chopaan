{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications, ScopedTypeVariables #-}
module SimNode where

import Lens.Micro
import Data.ProtoLens

import Control.Monad.Bayes.Class

import Chopaan.Node.NodeId
import Chopaan.Comm.Comm (MessageQs(..), initQs, Address(..), Dispatch(..), mkCallback, writeToPubQ)
import Chopaan.Comm.Mqtt (client, pub)
import Chopaan.Types (MQTTOpts(..), Options(..))
import Chopaan.Utils.Time


import Proto.NodeMessageSchema.NodeMessages
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as F

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans.State
import Data.Time
import Data.Time.Clock.Compat (NominalDiffTime)
import Data.Time.LocalTime.Compat (LocalTime, addLocalTime, diffLocalTime)

import Physics.Storage


import Dhall

import Streamly
import qualified Streamly.Prelude as S
import Streamly.Internal.Data.Stream.StreamK (hoist)
import Env.MonadEnv
import Chopaan.Kibbutz.Kibbutz (sensorKbtz, kbtz)

nodeStream :: (IsStream t) =>  t MonadEnv EnergyState
nodeStream = S.map snd $ S.iterateM (nodeStep @MonadEnv) (pure (startDay $ TimeOfDay 0 0 0, defMessage))

nodeStep :: forall m. (MonadSample m) => (LocalTime, EnergyState) -> m (LocalTime, EnergyState)
nodeStep (t, oldState) = do
  -- note that outflow of current is assumed to be positive 
  loadCurrent <- abs <$> normal 30 20
  gridCurrent <- normal 0 20
  solarCurrent <- biGauss daytime (30, 10) (0, 0.3) t
  --solarVoltage <- biGauss daytime (17, 3) (0, 1) t
  batteryVoltageDiff <- normal 0.01 0.001
  gridVoltageDiff <- normal 0.03 0.03 
  
  let
    t' = addLocalTime (1 :: NominalDiffTime) t
    batteryV = oldState ^. F.batteryVoltage + batteryVoltageDiff
    gridV = oldState ^. F.gridVoltage + gridVoltageDiff
    
    newState = (defMessage :: EnergyState)
      & F.batteryVoltage .~ batteryV
      & F.gridVoltage .~ gridV
      & F.batteryToLoadCurrent .~ loadCurrent
      & F.batteryToGridCurrent .~ (if gridCurrent > 0 then gridCurrent else 0)
      & F.gridToBatteryCurrent .~ (if gridCurrent < 0 then gridCurrent else 0)
      & F.solarInputCurrent    .~ solarCurrent
      & F.temperature          .~ (26 :: Double)
      & F.cpuTime             .~  timeToUIntSeconds t
  return $ (t', newState)
  where
    biGauss :: (MonadSample m) => (t -> Bool) -> (Double, Double) -> (Double, Double) -> t -> m Double 
    biGauss choice (mu, theta) (mu', theta') chooser = case choice chooser of
      True -> normal mu theta
      False -> normal mu' theta'
    daytime :: LocalTime -> Bool
    daytime t = t' > sunrise && t' < sunset
      where
        t' = localTimeOfDay t
    (sunrise, sunset) = (TimeOfDay 6 0 0, TimeOfDay 18 0 0)

startDay :: TimeOfDay -> LocalTime
startDay = LocalTime $ fromGregorian 1 1 2020
    
runtimeDist :: (MonadSample m) => m RuntimeStats
runtimeDist = return defMessage 

logsDist :: (MonadSample m) => m MeshFrame
logsDist = return defMessage



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

shift :: forall t. (IsStream t) => (forall a. t MonadEnv a -> t IO a)
shift = hoist sampleIOE

--testKbtz :: forall t. (IsStream t) => _
testKbtz = kbtz @t @MonadEnv


testClient :: IO ()
testClient = do
  Options{mqttOpts} <- input auto "./options.dhall"
  qs <- atomically $ initQs
  mqc <- client (mqttOpts {connId = "simulatedPub"}) (mkCallback qs)
  _ <- forkIO $ forever $ pub mqc (outbox qs)
  forever $ do
    S.mapM_ (\es -> writeToPubQ (outbox qs) (undefined :: NodeMAC) $ frame es) $ shift nodeStream
