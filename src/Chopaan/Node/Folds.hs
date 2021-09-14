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
, StrictData
#-}
module Chopaan.Node.Folds where

import qualified Streamly.Data.Fold as FL
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Data.Fold.Tee as FL

import Data.Time
import Data.Bifunctor
import Numeric.Estimator (KalmanFilter(..))

#ifndef ghcjs_HOST_OS
import ConCat.Misc (R)
#endif

import Chopaan.Node.NodeId (NodeMAC)
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
meshFold :: forall m. Monad m => NodeMAC -> FL.Fold m (RuntimeStats) (MeshNode, RxSignal)
meshFold = meshF
{-# INLINE meshFold #-}

timeFold :: forall m. Monad m => FL.Fold m (EnergyState) Timestamp
timeFold = FL.foldl' step' begin'
  where
    {-# INLINE step'#-}
    step' :: Timestamp -> EnergyState -> Timestamp
    step' (Nothing, _) cur = (Just tn, diffUTC tn tn)
      where
        tn = utcTimeES cur
    step' ((Just !prev), _) cur = (Just tn, diffUTC tn prev)
      where
        tn = utcTimeES cur
    {-# INLINE begin' #-}
    begin' :: Timestamp
    begin' = (Nothing, 0)



powerFold :: forall m. Monad m => FL.Fold m EnergyState (PowerNR)
powerFold = FL.foldl' (\_ !b -> power b) mempty 
{-# INLINE powerFold #-}

energyFold :: forall m. Monad m => FL.Fold m (EnergyState) (EnergyNR)
energyFold = fmap fst $ FL.foldl' step begin
  where
    -- forall s. Fold (s -> a -> m s) (m s) (s -> m b)
    {-# INLINE step #-}
    step :: (EnergyNR, Maybe UTCTime) -> EnergyState -> (EnergyNR, Maybe UTCTime)
    step (!esPrev, (Just !tPrev)) cur =
      (esPrev <> eAtT (power cur) (diffUTC tn tPrev), Just tn)
      where
        !tn = utcTimeES cur
    step (!esPrev, Nothing) cur =
      (esPrev <> (eAtT (power cur) 0), Just tn)
      where
        !tn = utcTimeES cur
    {-# INLINE begin #-}
    begin :: (EnergyNR, Maybe UTCTime)
    begin = (mempty, Nothing)
    {-# INLINE eAtT #-}
    eAtT :: PowerNR -> DiffTime -> (EnergyNR)
    eAtT !p !t = Node { tx = (pToE t tx)
                    , consumed = (pToE t consumed)
                    , generated = (pToE t generated)
                    }
      where
        Node{..} = p
{-# INLINE energyFold #-}

batteryFold :: forall m e p. (Monad m)
  => BatteryParams R -> FL.Fold m EnergyState (Battery WattSeconds Watts)
batteryFold !bat@BatteryParams{} = fmap (bimap toWattSeconds toWatts) $ fmap end $ FL.foldl' step begin
  where
    {-# INLINE step #-}
    step :: (Maybe UTCTime, Maybe (KF R))
      -> EnergyState
      -> (Maybe UTCTime, Maybe (KF R))
    step (!t, !pkf) !sensorReadings = let
        (!kf, _) = runEstimator bat (tdiff t) (storageSensors sensorReadings)
          (guestimateInitialSOC pkf)
      in (Just tnow, Just kf)
      where
        guestimateInitialSOC (Just !k) = k
        guestimateInitialSOC Nothing = initKF (initDynamic {
          soC = ocvToSoC bat (sensorTerminalV . storageSensors $ sensorReadings)
          })
        !tnow = utcTimeES sensorReadings
        tdiff (!Just t') = realToFrac $ diffUTCTime tnow t'
        tdiff Nothing = 0
    {-# INLINE begin #-}
    begin :: (Maybe UTCTime, Maybe (KF R))
    begin = (Nothing, Nothing)
    {-# INLINE end #-}
    end :: (Maybe UTCTime, Maybe (KF R)) -> Battery R R
    end (_, (!Just (KalmanFilter (StateVector{..}) _))) = (emptyB @R @R)
        { soc = clamp 0 99.9 soC
        , totalCapacity = chargeCapacity bat
        }
    end (_, (Nothing)) = emptyB @R @R
{-# INLINE batteryFold #-}


sensorFold :: forall m. (Monad m) => FL.Fold m (EnergyState) (SensorMetrics WattSeconds Watts) 
sensorFold = FL.toFold $ SensorMetrics
             <$> FL.Tee (fst <$> timeFold)
             <*> FL.Tee (snd <$> timeFold)
             <*> FL.Tee powerFold
             <*> FL.Tee energyFold
             <*> FL.Tee (batteryFold defBatteryParams)
             <*> FL.Tee demandFold
             <*> FL.Tee sensors 
{-# INLINE sensorFold #-}

demandFold :: (Monad m) => FL.Fold m (EnergyState) WattSeconds
demandFold = FL.foldl' (\_ nes -> (d $ power nes)) 0
  where
    {-# INLINE d #-}
    d (!Node{..}) = pToE horizon consumed
    horizon = (60 * 10)
{-# INLINE demandFold #-}


sensors :: (Monad m) => FL.Fold m EnergyState EnergyState
sensors = FL.foldl' (flip const) zeroMsg 
{-# INLINE sensors #-}

type SensorR = SensorMetrics WattSeconds Watts 

defSensorR :: SensorR
defSensorR = SensorMetrics Nothing 0 mempty mempty mempty 0 zeroMsg
{-# INLINE defSensorR #-}
