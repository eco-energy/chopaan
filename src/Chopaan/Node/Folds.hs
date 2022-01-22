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
, MultiParamTypeClasses
, FunctionalDependencies
, OverloadedLabels
, FlexibleContexts
#-}
module Chopaan.Node.Folds where

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Refold.Type as RF
import qualified Streamly.Data.Fold.Tee as FL

import GHC.Generics hiding (R)
import Data.Generics.Product
import Data.Time
import Data.Bifunctor
import Numeric.Estimator (KalmanFilter(..))
import Control.Monad.Bayes.Class hiding (gamma)

import ConCat.Misc (R)

import Chopaan.Node.NodeId (NodeMAC)
import Chopaan.Node.NodeSensors (fromNodeMessage, NodeT')
import Chopaan.Node.HW (HW(..))
import qualified Chopaan.Node.Components as Comp
import Chopaan.Node.Storage
import Chopaan.Node.Storage.Battery
import Chopaan.Node.Metrics
import Chopaan.Utils.Time

import Proto.NodeMessageSchema.NodeMessages (EnergyState, RuntimeStats)

import Chopaan.Node.Mesh

{----------------------------------------------------------------------------------------------------


Folds of type FL.Fold, as functions to the instantatenous values of the system over an indexed set.
                      :: forall s. Fold (s -> a -> m s) (m s) (s -> m b)


-----------------------------------------------------------------------------------------------------}

type MeshR = (MeshNode, RxSignal)

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
    eAtT :: PowerNR -> NominalDiffTime -> (EnergyNR)
    eAtT !p !t = Node { tx = (pToE t tx)
                    , consumed = (pToE t consumed)
                    , generated = (pToE t generated)
                    }
      where
        Node{..} = p
{-# INLINE energyFold #-}

type Unop a = a -> a

batteryFold :: forall m e p. (MonadSample m)
  => (Comp.BatteryTop R) -> BatteryParams R -> FL.Fold m EnergyState (Battery WattSeconds Watts)
batteryFold cBat !bat@BatteryParams{} = fmap (bimap toWattSeconds toWatts)
  $ fmap (flip end emptyB)
  $ FL.foldlM' step begin
  where
    {-# INLINE step #-}
    step :: (Maybe UTCTime, Maybe (KF R))
      -> EnergyState
      -> m (Maybe UTCTime, Maybe (KF R))
    step (!t, !pkf) !sensorReadings = (\kf -> return (Just tnow, Just (fst kf)))
      =<< (estimatorStep bat (tdiff t) (storageSensors sensorReadings) (initialSOC' pkf))
      where
        initialSOC' (Just !k) = k
        initialSOC' Nothing = initialSOC bat (sensorTerminalV . storageSensors $ sensorReadings)
        !tnow = utcTimeES sensorReadings
        tdiff (!Just t') = realToFrac $ diffUTCTime tnow t'
        tdiff Nothing = 0
    {-# INLINE begin #-}
    begin :: m (Maybe UTCTime, Maybe (KF R))
    begin = pure (Nothing, Nothing)
    {-# INLINE end #-}
    end :: (Maybe UTCTime, Maybe (KF R)) -> Unop (Battery R R)
    end (_, (!Just kf)) = \b -> runKF b kf
    end (_, (Nothing)) = const (emptyB @R @R)
{-# INLINE batteryFold #-}

newtype Likelihood = Likelihood Double

class FilterState s a | s -> a where
  initialState :: s
  singleStep :: a -> Filter s a
  likelihood :: s -> a -> Likelihood

type Filter s a = s -> a -> (s, a)

filterF :: forall m s a. (Monad m, Monoid a) => Int -> Filter s a -> s -> FL.Fold m a (s, a)
filterF session f s = (FL.take session (FL.mkFold_ step start))
  where
    start :: FL.Step (s, a) (s, a)
    start = FL.Partial (s, mempty) 
    step :: (s, a) -> a -> FL.Step (s, a) (s, a)
    step (s', _) a''' = FL.Partial (f s' a''')


sensorR :: HW R -> SensorR -> EnergyState -> SensorR
sensorR hw s n = undefined

sensorFold' :: forall m. (Monad m, MonadSample m) => FL.Fold m (EnergyState) (SensorMetrics WattSeconds Watts) 
sensorFold' = undefined
{-# INLINE sensorFold' #-}

sensorFold :: forall m. (Monad m, MonadSample m) => (HW R) -> FL.Fold m (EnergyState) (SensorMetrics WattSeconds Watts) 
sensorFold hw = FL.toFold $ SensorMetrics
                <$> FL.Tee (fst <$> timeFold)
                <*> FL.Tee (snd <$> timeFold)
                <*> FL.Tee powerFold
                <*> FL.Tee energyFold
                <*> FL.Tee (batteryFold (storage hw) defBatteryParams)
                <*> FL.Tee demandFold
                <*> FL.Tee sensors
{-# INLINE sensorFold #-}

sensorRefold :: forall m. (Monad m, MonadSample m)
  => HW R
  -> RF.Refold m SensorR (EnergyState) SensorR 
sensorRefold hw = RF.iterate (RF.foldl' (sensorR hw))
{-# INLINE sensorRefold #-}


demandFold :: (Monad m) => FL.Fold m (EnergyState) WattSeconds
demandFold = FL.foldl' (\_ nes -> (d $ power nes)) 0
  where
    {-# INLINE d #-}
    d (!Node{..}) = pToE horizon consumed
    horizon = (60 * 10)
{-# INLINE demandFold #-}


sensors :: (Monad m) => FL.Fold m EnergyState (NodeT' Double)
sensors = FL.foldl' (\_ e -> fromNodeMessage e) (fromNodeMessage zeroMsg) 
{-# INLINE sensors #-}

type SensorR = SensorMetrics WattSeconds Watts 

defSensorR :: SensorR
defSensorR = SensorMetrics Nothing 0 mempty mempty mempty 0 (fromNodeMessage zeroMsg)
{-# INLINE defSensorR #-}


