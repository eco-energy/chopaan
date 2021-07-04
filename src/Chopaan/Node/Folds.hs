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
, CPP
#-}
module Chopaan.Node.Folds where

import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL

import Data.Time
import Data.Bifunctor
import Numeric.Estimator (KalmanFilter(..))

#ifndef ghcjs_HOST_OS
import ConCat.Misc (R)
#endif

import Chopaan.Node.Storage
import Chopaan.Node.Metrics
import Chopaan.Utils.Time

import Proto.NodeMessageSchema.NodeMessages (EnergyState, RuntimeStats)

import Chopaan.Node.Mesh


#ifdef ghcjs_HOST_OS
type R = Double
#endif

{----------------------------------------------------------------------------------------------------


Folds of type FL.Fold, as functions to the instantatenous values of the system over an indexed set.
                      :: forall s. Fold (s -> a -> m s) (m s) (s -> m b)


-----------------------------------------------------------------------------------------------------}
meshFold :: forall m. Applicative m => FL.Fold m (RuntimeStats) (MeshNode, RxSignal)
meshFold = meshF
{-# INLINE meshFold #-}

timeFold :: forall m. Applicative m => FL.Fold m (EnergyState) Timestamp
timeFold = FL.mkFold step' begin' done'
  where
    {-# INLINE step'#-}
    step' :: Timestamp -> EnergyState -> m (Timestamp)
    step' (Nothing, _) cur = pure $ (Just tn, diffUTC tn tn)
      where
        tn = utcTimeES cur
    step' ((Just !prev), _) cur = pure $ (Just tn, diffUTC tn prev)
      where
        tn = utcTimeES cur
    {-# INLINE begin' #-}
    begin' :: m Timestamp
    begin' = pure (Nothing, 0)
    {-# INLINE done' #-}
    done' :: Timestamp -> m Timestamp
    done' = pure


powerFold :: forall m. Applicative m => FL.Fold m EnergyState (PowerNR)
powerFold = FL.mkFold (\_ b-> pure $ power b) (pure $ mempty) pure 
{-# INLINE powerFold #-}

energyFold :: forall m. Applicative m => FL.Fold m (EnergyState) (EnergyNR)
energyFold = FL.mkFold step begin end
  where
    -- forall s. Fold (s -> a -> m s) (m s) (s -> m b)
    {-# INLINE step #-}
    step :: (EnergyNR, Maybe UTCTime) -> EnergyState -> m (EnergyNR, Maybe UTCTime)
    step (esPrev, (Just tPrev)) cur = pure $
      (esPrev <> eAtT (power cur) (diffUTC tn tPrev), Just tn)
      where
        tn = utcTimeES cur
    step (esPrev, Nothing) cur = pure $
      (esPrev <> (eAtT (power cur) 0), Just tn)
      where
        tn = utcTimeES cur
    {-# INLINE begin #-}
    begin :: m (EnergyNR, Maybe UTCTime)
    begin = pure $ (mempty, Nothing)
    {-# INLINE end#-}
    end :: (EnergyNR, Maybe UTCTime) -> m (EnergyNR)
    end = pure . fst
    {-# INLINE eAtT #-}
    eAtT :: PowerNR -> DiffTime -> (EnergyNR)
    eAtT p t = Node { tx = (pToE t tx)
                    , consumed = (pToE t consumed)
                    , generated = (pToE t generated)
                    }
      where
        Node{..} = p
{-# INLINE energyFold #-}

batteryFold :: forall m e p. (Monad m)
  => BatteryParams R -> FL.Fold m EnergyState (Battery WattSeconds Watts)
batteryFold bat@BatteryParams{} = fmap (bimap toWattSeconds toWatts) $ FL.mkFold step begin end
  where
    {-# INLINE step #-}
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
    {-# INLINE begin #-}
    begin :: m (Maybe UTCTime, Maybe (KF R))
    begin = pure $ (Nothing, Nothing)
    {-# INLINE end #-}
    end :: (Maybe UTCTime, Maybe (KF R)) -> m (Battery R R)
    end (_, (Just (KalmanFilter (StateVector{..}) _))) = pure $
      (emptyB @R @R)
        { soc = clamp 0 99.9 soC
        , totalCapacity = chargeCapacity bat
        }
    end (_, (Nothing)) = pure (emptyB @R @R)
{-# INLINE batteryFold #-}


sensorFold :: forall m. (Monad m) => FL.Fold m (EnergyState) (SensorMetrics WattSeconds Watts) 
sensorFold = SensorMetrics
             <$> (fst <$> timeFold)
             <*> (snd <$> timeFold)
             <*> powerFold
             <*> energyFold
             <*> (batteryFold defBatteryParams)
             <*> demandFold 
{-# INLINE sensorFold #-}

demandFold :: (Applicative m) => FL.Fold m (EnergyState) WattSeconds
demandFold = FL.Fold (\_ nes -> pure $ (d $ power nes)) (pure 0) pure
  where
    {-# INLINE d #-}
    d (Node{..}) = pToE horizon consumed
    horizon = (60 * 10)
{-# INLINE demandFold #-}


sensors :: (Applicative m) => FL.Fold m EnergyState EnergyState
sensors = FL.Fold (\_ nes -> pure nes) (pure zeroMsg) (pure) 

type SensorR = SensorMetrics WattSeconds Watts 

defSensorR :: SensorR
defSensorR = SensorMetrics Nothing 0 mempty mempty mempty 0
