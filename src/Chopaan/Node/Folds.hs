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



{------------------ Basic Types -----------------------}



newtype WattSeconds = WS { unWs :: Compensated Double } deriving (Eq, Ord, Num, Generic, Fractional, Real, RealFrac)

newtype Watts = W { unW :: Compensated Double } deriving (Eq, Ord, Num, Generic, Fractional, Real, RealFrac)

instance Show WattSeconds where
  show = (printf ("%.2g")) . fromWattSeconds

instance Show Watts where
  show = (printf ("%.2g")) . fromWatts


fromWatts :: Watts -> Double
fromWatts = uncompensated . unW

fromWattSeconds :: WattSeconds -> Double
fromWattSeconds = uncompensated . unWs

toWatts :: Double -> Watts
toWatts a = W $ add a 0 compensated

toWattSeconds :: Double -> WattSeconds
toWattSeconds a = WS $ add a 0 compensated


instance ToField (Watts) where
  toField = toField . uncompensated . unW

instance ToField (WattSeconds) where
  toField = toField . uncompensated . unWs

instance ToJSON WattSeconds where
  toJSON = toJSON . uncompensated . unWs

instance ToJSON Watts where
  toJSON = toJSON . uncompensated . unW

instance FromJSON WattSeconds where
  parseJSON x = toWattSeconds <$> (A.parseJSON x)

instance FromJSON Watts where
  parseJSON x = toWatts <$> (A.parseJSON x)



{----------------------------------------------------------------------------------------------------


Folds of type FL.Fold, as functions to the instantatenous values of the system over an indexed set.
                      :: forall s. Fold (s -> a -> m s) (m s) (s -> m b)


-----------------------------------------------------------------------------------------------------}

timeFold :: forall m. Monad m => FL.Fold m (EnergyState) Timestamp
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



powerFold :: forall m. (Monad m) => FL.Fold m EnergyState (Power Watts)
powerFold = FL.Fold (\_ b-> pure $ power' b) (pure $ mempty) return 


energyFold :: forall m. (Monad m) => FL.Fold m (EnergyState) (Energy WattSeconds)
energyFold = (FL.Fold step begin end)
  where
    -- forall s. Fold (s -> a -> m s) (m s) (s -> m b)
    step :: (Energy WattSeconds, Maybe UTCTime) -> EnergyState -> m (Energy WattSeconds, Maybe UTCTime)
    step (esPrev, (Just tPrev)) cur = pure $ ((esPrev <> (eAtT (power' cur) (Just tn, diffUTC tn tPrev))), Just tn)
      where
        tn = utcTimeES cur
    step (esPrev, Nothing) cur = pure $ ((esPrev <> (eAtT (power' cur) (Just tn, diffUTC tn tn))), Just tn)
      where
        tn = utcTimeES cur
    begin :: m (Energy WattSeconds, Maybe UTCTime)
    begin = pure $ (mempty, Nothing)
    end :: (Energy WattSeconds, Maybe UTCTime) -> m (Energy WattSeconds)
    end = pure . fst
    eAtT :: Power Watts -> Timestamp -> (Energy WattSeconds)
    eAtT p (_, t) = Energy { txIn = (pToE t tInP)
                           , txOut = (pToE t tOutP)
                           , consumed = (pToE t loadP)
                           , generated = (pToE t genP)}
      where
        Power{..} = p

pToE :: (Real t) => t -> Watts -> WattSeconds
pToE t (W p') = WS $ (realToFrac t) *^ p'

batteryFold :: forall m. (MonadIO m) => BatteryParams R -> FL.Fold m EnergyState (Battery R R)
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
    begin = return $ (Nothing, Nothing)
    end :: (Maybe UTCTime, Maybe (KF R)) -> m (Battery R R)
    end (_, Just (KalmanFilter (StateVector{..}) _)) = return $
      (emptyB @R @R) { soc = soC
                     , totalCapacity = chargeCapacity bat
                     }
    end (_, Nothing) = return $ emptyB @R @R


nodeMonitor' :: forall m. (MonadIO m) => (EnergyState -> IO ()) -> FL.Fold m (EnergyState) (NodeMetrics WattSeconds Watts) 
nodeMonitor' save = NodeMetrics <$> (fst <$> tn) <*> (snd <$> tn) <*> powerFold <*> en <*> sensors <*> (batteryFold defBatteryParams) <*> demandFold 
  where
    tn :: FL.Fold m (EnergyState) Timestamp
    tn = timeFold
    en :: FL.Fold m (EnergyState) (Energy WattSeconds)
    en =  energyFold
    sensors :: FL.Fold m (EnergyState) (EnergyState)
    sensors = FL.Fold (\_ nes -> do
                          liftIO $ save nes
                          return nes
                      ) (pure zeroMsg) (pure) 
    demandFold :: FL.Fold m (EnergyState) WattSeconds
    demandFold = FL.Fold (\_ nes -> pure (d $ power' nes)) (pure 0) pure
      where
        d (Power{..}) = pToE (60 * 10) loadP


nodeMonitor'' :: forall m. (MonadIO m) => DBOpts -> NodeMAC -> m (FL.Fold m EnergyState NodeS)
nodeMonitor'' dbOpts n = do
  conn <- (liftIO $ getDbConn dbOpts)
  return $ nodeMonitor' $ insertEnergyState conn n

nodeMonitor :: forall m. (MonadIO m) => FL.Fold m (EnergyState) (NodeMetrics WattSeconds Watts) 
nodeMonitor = nodeMonitor' save
  where save _ = print "x"

power' :: EnergyState -> Power Watts 
power' = (toWatts <$>) . power

type NodeS = NodeMetrics WattSeconds Watts 

defNodeS :: NodeS
defNodeS = NodeMetrics Nothing 0 mempty mempty zeroMsg mempty 0
