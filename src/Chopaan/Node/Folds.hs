{-#
LANGUAGE ScopedTypeVariables
, TypeApplications
, BangPatterns
, DeriveGeneric
, RecordWildCards
, GeneralizedNewtypeDeriving
, ExistentialQuantification
, RankNTypes
, QuantifiedConstraints
#-}
module Chopaan.Node.Folds where

import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL

import Data.Time
import Data.Bifunctor
import Numeric.Estimator (KalmanFilter(..))
import ConCat.Misc (R)


import Chopaan.Node.Storage
import Chopaan.Node.Metrics
import Chopaan.Utils.Time

import Proto.NodeMessageSchema.NodeMessages (EnergyState, RuntimeStats)

import Chopaan.Node.Mesh

{----------------------------------------------------------------------------------------------------


Folds of type FL.Fold, as functions to the instantatenous values of the system over an indexed set.
                      :: forall s. Fold (s -> a -> m s) (m s) (s -> m b)


-----------------------------------------------------------------------------------------------------}
meshFold :: forall m. Applicative m => FL.Fold m (RuntimeStats) (MeshNode, RxSignal)
meshFold = meshF


timeFold :: forall m. Applicative m => FL.Fold m (EnergyState) Timestamp
timeFold = FL.mkFold step' begin' done'
  where
    step' :: Timestamp -> EnergyState -> m (Timestamp)
    step' (Nothing, _) cur = pure $ (Just tn, diffUTC tn tn)
      where
        tn = utcTimeES cur
    step' ((Just !prev), _) cur = pure $ (Just tn, diffUTC tn prev)
      where
        tn = utcTimeES cur
    begin' :: m Timestamp
    begin' = pure (Nothing, 0)
    done' :: Timestamp -> m Timestamp
    done' = pure

newtype AppF m a b = AppF { unAppF :: FL.Fold m a b } deriving (Functor)

instance (Monad m) => Applicative (AppF m a) where
  pure = AppF . pure
  (AppF f) <*> (AppF xs) = AppF $ f <*> xs

powerFold :: forall m. Applicative m => AppF m EnergyState (PowerNR)
powerFold = AppF (FL.mkFold (\_ b-> pure $ power b) (pure $ mempty) pure) 


energyFold :: forall m. Applicative m => AppF m (EnergyState) (EnergyNR)
energyFold = AppF (FL.mkFold step begin end)
  where
    -- forall s. Fold (s -> a -> m s) (m s) (s -> m b)
    step :: (EnergyNR, Maybe UTCTime) -> EnergyState -> m (EnergyNR, Maybe UTCTime)
    step (esPrev, (Just tPrev)) cur = pure $
      (esPrev <> eAtT (power cur) (diffUTC tn tPrev), Just tn)
      where
        tn = utcTimeES cur
    step (esPrev, Nothing) cur = pure $
      (esPrev <> (eAtT (power cur) 0), Just tn)
      where
        tn = utcTimeES cur
    begin :: m (EnergyNR, Maybe UTCTime)
    begin = pure $ (mempty, Nothing)
    end :: (EnergyNR, Maybe UTCTime) -> m (EnergyNR)
    end = pure . fst
    eAtT :: PowerNR -> DiffTime -> (EnergyNR)
    eAtT p t = Node { tx = (pToE t tx)
                    , consumed = (pToE t consumed)
                    , generated = (pToE t generated)
                    }
      where
        Node{..} = p

batteryFold :: forall m e p. (Monad m)
  => BatteryParams R -> FL.Fold m EnergyState (Battery WattSeconds Watts)
batteryFold bat@BatteryParams{} = fmap (bimap toWattSeconds toWatts) $ FL.mkFold step begin end
  where
    step :: (Maybe UTCTime, Maybe (KF R))
      -> EnergyState
      -> m (Maybe UTCTime, Maybe (KF R))-- (Battery R R))
    step (t, pkf) sensorReadings = do
      let
        (kf, ki) = runEstimator bat (tdiff t) (storageSensors sensorReadings)
          (guestimateInitialSOC pkf)
      pure $ (Just tnow, Just kf)
      where
        guestimateInitialSOC (Just k) = k
        guestimateInitialSOC Nothing = initKF (initDynamic {
          soC = ocvToSoC bat (sensorTerminalV . storageSensors $ sensorReadings)
          })
        tnow = utcTimeES sensorReadings
        tdiff (Just t') = realToFrac $ diffUTCTime tnow t'
        tdiff Nothing = 0
        
    begin :: m (Maybe UTCTime, Maybe (KF R))
    begin = pure $ (Nothing, Nothing)
    end :: (Maybe UTCTime, Maybe (KF R)) -> m (Battery R R)
    end (_, (Just (KalmanFilter (StateVector{..}) _))) = pure $
      (emptyB @R @R)
        { soc = clamp 0 99.9 soC
        , totalCapacity = chargeCapacity bat
        }
    end (_, (Nothing)) = pure (emptyB @R @R)



sensorFold :: forall m. (Monad m) => FL.Fold m (EnergyState) (SensorMetrics WattSeconds Watts) 
sensorFold = SensorMetrics
             <$> (fst <$> timeFold)
             <*> (snd <$> timeFold)
             <*> unAppF powerFold
             <*> unAppF energyFold
             <*> (batteryFold defBatteryParams)
             <*> demandFold 
    
demandFold :: (Applicative m) => FL.Fold m (EnergyState) WattSeconds
demandFold = FL.Fold (\_ nes -> pure $ (d $ power nes)) (pure 0) pure
  where
    d (Node{..}) = pToE horizon consumed
    horizon = (60 * 10)

sensors :: (Applicative m) => FL.Fold m EnergyState EnergyState
sensors = FL.Fold (\_ nes -> pure nes) (pure zeroMsg) (pure) 

type SensorR = SensorMetrics WattSeconds Watts 

defSensorR :: SensorR
defSensorR = SensorMetrics Nothing 0 mempty mempty mempty 0
