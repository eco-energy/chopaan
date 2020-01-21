{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Node where


import qualified Data.Time as Time
import Data.Time.Clock.POSIX

-- Vis
import qualified Text.PrettyPrint.Tabulate as PPT
import GHC.Generics (Generic)
import Data.Data

import Proto.NodeMessages
import Proto.NodeMessages_Fields

import Lens.Micro

import Streamly
import qualified Streamly.Prelude as S

import qualified Data.Set as Set 

----------------------------------------------------------------------------------
-- Metric Tracking

-- Our Scalars
type WattSeconds = Double

type Watts = Double

type Demand = Double

newtype NodeId a = NodeId { unNodeId :: a } deriving (Eq, Show, Ord, Data, Generic)

instance (Show a) => PPT.CellValueFormatter (NodeId a)

-- Episodic Metrics

data EnergyBalance = EnergyBalance
  { txIn :: WattSeconds
  , txOut :: WattSeconds
  , consumed :: WattSeconds
  , generated :: WattSeconds
  } deriving (Eq, Show, Ord, Generic, Data)

initEA :: EnergyBalance
initEA = EnergyBalance 0 0 0 0

-- Check associativity
instance Semigroup EnergyBalance where
  v1 <> v2 = EnergyBalance
    { txIn = txIn v1 + txIn v2
    , txOut = txOut v1 + txOut v2
    , consumed = consumed v1 + consumed v2
    , generated = generated v1 + generated v2
    }

instance Monoid EnergyBalance where
  mempty = initEA

instance PPT.Tabulate EnergyBalance PPT.ExpandWhenNested

data NodeMetrics a = NodeMetrics
  { _lastW :: Time.NominalDiffTime
  , _loss :: a
  , _stored :: a
  , _demand :: a
  } deriving (Eq, Ord, Show, Generic, Data)

instance PPT.Tabulate (NodeMetrics a) PPT.ExpandWhenNested


-- Streams over T, one for each n.
stored :: (IsStream s, Monad m) => s m EnergyBalance -> s m WattSeconds
stored es = S.scanl' stored' 0 es
  where
    stored' :: WattSeconds -> EnergyBalance -> WattSeconds
    stored' s' (EnergyBalance{..}) = s' +
                                   (generated + ((withLossFrac cLoss) txIn))
                                   - ((withLossFrac dLoss txOut) + (withLossFrac cLoss) consumed)
    withLossFrac a = ((1 + a) *)
    cLoss = 0.01
    dLoss = 0.1

demand :: (IsStream s, Monad m) => s m EnergyBalance -> s m WattSeconds
demand es = S.map demand' es
  where
    demand' :: EnergyBalance -> WattSeconds
    demand' EnergyBalance{..} = (consumed - generated)

-- Imagine we're getting the energy audits of all the nodes for a certain window,
-- and that we have to calculate thesum txIn across all nodes
loss :: (IsStream s, Monad m) => s m EnergyBalance -> s m WattSeconds
loss es = (S.scanl' nLoss 0 es)
  where
    nLoss :: WattSeconds -> EnergyBalance -> WattSeconds
    nLoss l' (EnergyBalance {txOut, txIn}) = l' + (txOut - txIn)

lastWait :: (IsStream t, Monad m) => Time.UTCTime -> t m EnergyState -> t m Time.NominalDiffTime
lastWait t es = S.map snd $ S.scanl' sf (t, 0 :: Time.NominalDiffTime) es
  where
    sf :: (Time.UTCTime, Time.NominalDiffTime) -> EnergyState -> (Time.UTCTime, Time.NominalDiffTime)
    sf (ptime, _) e = (utcTNow e, Time.diffUTCTime (utcTNow e) ptime)


data Power = R

newtype PowerS m a = PowerS { unPS :: (Monad m, Fractional a) => SerialT m Power }

newtype EnergyS m = EnergyS {unES :: Monad m => SerialT m EnergyState }

runES s = S.scanl' scanToRecord unES s
  where
    scanToRecord = undefined

--powerBalance :: (Set.Set (EnergyS m)) -> PowerS m a 
--powerBalance es = S.concatMap `wAsync` (\s_t -> s_t ^. batteryVoltage * s_t ^. (gridToBatteryCurrent)) es

audit :: (IsStream t, Monad m) => Time.UTCTime -> t m EnergyState -> t m (Time.UTCTime, EnergyBalance)
audit t es' = S.scanl' audit' (t, initEA) es'
  where
    audit' :: (Time.UTCTime, EnergyBalance) -> EnergyState -> (Time.UTCTime, EnergyBalance)
    audit' (t', prevEb) es = (utcTNow es, prevEb <> EnergyBalance txIn' txOut' cnsm' gen')
      where
        txIn' :: WattSeconds
        txIn' = (es ^. batteryVoltage :: Double) * (es ^. gridToBatteryCurrent :: Double) 
        txOut' :: WattSeconds
        txOut' = (es ^. batteryVoltage :: Double) * (es ^. batteryToGridCurrent :: Double)
        cnsm' :: WattSeconds
        cnsm' = (es ^. batteryVoltage :: Double) * (es ^. batteryToLoadCurrent :: Double)
        gen' :: WattSeconds
        gen' = (es ^. batteryVoltage :: Double) * (es ^. solarInputCurrent :: Double) * (realToFrac (integralMultiplier))
        integralMultiplier :: Time.NominalDiffTime
        integralMultiplier = Time.diffUTCTime (utcTNow es) t'

    
utcTNow :: EnergyState -> Time.UTCTime 
utcTNow es = posixSecondsToUTCTime $ fromIntegral $ es ^. cpuTime

-- negative is 
batteryCurrent :: EnergyState -> Double
batteryCurrent es = i - o
  where o = es ^. batteryToGridCurrent + es ^. batteryToLoadCurrent
        i = es ^. gridToBatteryCurrent + es ^. solarInputCurrent


--nodeMonitor :: t IO EnergyState -> t IO EnergyState
runNodeMonitor :: (Monad (s IO), IsStream s) => s IO EnergyState -> s IO (NodeMetrics WattSeconds, EnergyBalance)
runNodeMonitor es = do
  initTime <- S.yieldM Time.getCurrentTime
  (t', eb) <- audit initTime es
  t <- lastWait initTime es
  l <- (loss . (S.map snd)) $ audit initTime es
  s <- (stored . (S.map snd)) $ audit initTime es
  d <- (demand . (S.map snd)) $ audit initTime es
  let
    nm = NodeMetrics t l s d
  S.yieldM $ (putStrLn $ "Energy Balance@" <> show t' <> "  " <> show eb)
  S.yieldM $ (putStrLn $ "Node Metrics@" <> show t' <> "  " <> show nm)
  return $ (nm, eb)

