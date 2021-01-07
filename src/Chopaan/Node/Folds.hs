{-#
LANGUAGE ScopedTypeVariables
, TypeApplications
, BangPatterns
, DeriveGeneric
, RecordWildCards
, GeneralizedNewtypeDeriving
#-}
module Chopaan.Node.Folds where

import GHC.Generics hiding (R)


import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL


import Data.Time
import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Aeson as A
import Data.Csv hiding ((.:))
import Numeric.Estimator (KalmanFilter(..))
import Numeric.Compensated
import ConCat.Misc (R)
import Text.Printf


import Control.Monad.IO.Class

import Chopaan.Node.NodeId
import Chopaan.Node.Storage
import Chopaan.Node.Metrics
import Chopaan.Types (DBOpts)
import Chopaan.Utils.Time

import Chopaan.DB
import Chopaan.DB.Sensors


import Proto.NodeMessageSchema.NodeMessages

{----------------------------------------------------------------------------------------------------


Folds of type FL.Fold, as functions to the instantatenous values of the system over an indexed set.
                      :: forall s. Fold (s -> a -> m s) (m s) (s -> m b)


-----------------------------------------------------------------------------------------------------}

timeFold :: forall m. Applicative m => FL.Fold m (EnergyState) Timestamp
timeFold = FL.Fold step' begin' done'
  where
    step' :: (Timestamp -> EnergyState -> m Timestamp)
    step' (Nothing, _) cur = pure (Just tn, diffUTC tn tn)
      where
        tn = utcTimeES cur
    step' ((Just !prev), _) cur = pure (Just tn, diffUTC tn prev)
      where
        tn = utcTimeES cur
    begin' :: m Timestamp
    begin' = pure (Nothing, 0)
    done' :: Timestamp -> m Timestamp
    done' = pure



powerFold :: forall m. (Applicative m) => FL.Fold m EnergyState (Power)
powerFold = FL.Fold (\_ b-> pure $ power b) (pure $ mempty) pure 


energyFold :: forall m. (Applicative m) => FL.Fold m (EnergyState) (Energy)
energyFold = (FL.Fold step begin end)
  where
    -- forall s. Fold (s -> a -> m s) (m s) (s -> m b)
    step :: (Energy, Maybe UTCTime) -> EnergyState -> m (Energy, Maybe UTCTime)
    step (esPrev, (Just tPrev)) cur = pure $
      (esPrev <> eAtT (power cur) (diffUTC tn tPrev), Just tn)
      where
        tn = utcTimeES cur
    step (esPrev, Nothing) cur = pure $
      (esPrev <> (eAtT (power cur) 0), Just tn)
      where
        tn = utcTimeES cur
    begin :: m (Energy, Maybe UTCTime)
    begin = pure $ (mempty, Nothing)
    end :: (Energy, Maybe UTCTime) -> m (Energy)
    end = pure . fst
    eAtT :: Power -> DiffTime -> (Energy)
    eAtT p t = Node { txIn = (pToE t txIn)
                         , txOut = (pToE t txOut)
                         , consumed = (pToE t consumed)
                         , generated = (pToE t generated)}
      where
        Node{..} = p

batteryFold :: forall m. (Monad m) => BatteryParams R -> FL.Fold m EnergyState (Battery R R)
batteryFold bat@BatteryParams{} = FL.Fold step begin end
  where
    step :: (Maybe UTCTime, Maybe (KF R)) -> EnergyState -> m (Maybe UTCTime, Maybe (KF R))
    step (t, kf) sensorReadings = ((\(_, b) -> (Just tnow, Just b)) . snd) <$>
        (runKalmanState (tdiff t) (cState kf) (runEstimator bat (tdiff t) $ storageSensors sensorReadings))
      where
        cState (Just (KalmanFilter currState _)) = currState
        cState Nothing = initDynamic {soC = ocvToSoC bat (sensorTerminalV . storageSensors $ sensorReadings)}
        tnow = utcTimeES sensorReadings
        tdiff (Just t') = realToFrac $ diffUTCTime tnow t'
        tdiff Nothing = 0
        
    begin :: m (Maybe UTCTime, Maybe (KF R))
    begin = pure $ (Nothing, Nothing)
    end :: (Maybe UTCTime, Maybe (KF R)) -> m (Battery R R)
    end (_, Just (KalmanFilter (StateVector{..}) _)) = pure $
      (emptyB @R @R) { soc = soC
                     , totalCapacity = chargeCapacity bat
                     }
    end (_, Nothing) = pure $ emptyB @R @R


sensorFold :: forall m. (Monad m) => FL.Fold m (EnergyState) (SensorMetrics WattSeconds Watts) 
sensorFold = SensorMetrics
             <$> (fst <$> timeFold)
             <*> (snd <$> timeFold)
             <*> powerFold
             <*> energyFold
             <*> sensors
             <*> (batteryFold defBatteryParams)
             <*> demandFold 
  where
    


demandFold :: (Applicative m) => FL.Fold m (EnergyState) WattSeconds
demandFold = FL.Fold (\_ nes -> pure (d $ power nes)) (pure 0) pure
  where
    d (Node{..}) = pToE (60 * 10) consumed

sensors :: (Applicative m) => FL.Fold m EnergyState EnergyState
sensors = FL.Fold (\_ nes -> pure nes) (pure zeroMsg) (pure) 



type SensorS = SensorMetrics WattSeconds Watts 

defSensorS :: SensorS
defSensorS = SensorMetrics Nothing 0 mempty mempty zeroMsg mempty 0
