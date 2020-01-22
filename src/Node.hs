{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Node (runNodeMonitor, NodeId(..), defaultES, Watts, unNodeId) where


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

import Data.ProtoLens (Message, defMessage)
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

data Power a = Power
  { gen :: a
  , tIn :: a
  , tOut :: a
  , load :: a }
  deriving (Eq, Ord, Show, Generic, Data, Functor, Applicative)

instance (Num a) => Semigroup (Power a) where
  p <> p' = (+) <$> p <*> p'

instance (Num a) => Monoid (Power a) where
  mempty = Power 0 0 0 0

data NodeMetrics e p = NodeMetrics
  { _lastW :: Time.NominalDiffTime
  , _loss :: e
  , _stored :: e
  , _demand :: e
  , _powerS :: Power p
  , _energyS :: EnergyBalance
  } deriving (Eq, Ord, Show, Generic, Data)


newtype NodeS m e p = NodeS { runNodeS :: (SerialT m (NodeMetrics e p)) } deriving (Generic)

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


nodeES :: (IsStream t, Monad m, Eq a) => NodeId a -> t m (NodeId a, EnergyState) -> t m EnergyState
nodeES node s = S.filter (\a-> fst a == node) s
                & S.map snd

powerAndEnergy :: (IsStream t, Monad m) => Time.UTCTime -> t m EnergyState -> t m (Time.UTCTime, (Power Watts, EnergyBalance))
powerAndEnergy t es' = S.scanl' audit' (t, (mempty, mempty)) es'
  where
    audit' :: (Time.UTCTime, (Power Watts, EnergyBalance)) -> EnergyState -> (Time.UTCTime, (Power Watts, EnergyBalance))
    audit' (t', (_, prevEb)) es = (utcTNow es, (pb, prevEb <> eb))
      where
        eb = EnergyBalance (fst txIn') (fst txOut') (fst cnsm') (fst gen')
        pb = Power (snd txIn') (snd txOut') (snd cnsm') (snd gen')
        txIn' :: (Watts, WattSeconds)
        txIn' = integrate $ p batteryVoltage gridToBatteryCurrent 
        txOut' :: (Watts, WattSeconds)
        txOut' = integrate $ p batteryVoltage batteryToGridCurrent
        cnsm' :: (Watts, WattSeconds)
        cnsm' = integrate $ p batteryVoltage batteryToLoadCurrent
        gen' :: (Watts, WattSeconds)
        gen' = integrate $ p batteryVoltage solarInputCurrent
        delT = realToFrac $ Time.diffUTCTime (utcTNow es) t'
        p v i = es ^. v * es ^. i
        integrate p' = (p', p' * delT)

utcTNow :: EnergyState -> Time.UTCTime 
utcTNow es = posixSecondsToUTCTime $ fromIntegral $ es ^. cpuTime

-- negative is 
batteryCurrent :: EnergyState -> Double
batteryCurrent es = i - o
  where o = es ^. batteryToGridCurrent + es ^. batteryToLoadCurrent
        i = es ^. gridToBatteryCurrent + es ^. solarInputCurrent


--nodeMonitor :: t IO EnergyState -> t IO EnergyState
runNodeMonitor :: (Eq a, Monad (s IO), IsStream s) => NodeId a -> s IO (NodeId a, EnergyState) -> s IO (NodeMetrics WattSeconds Watts)
runNodeMonitor n allEs = do
  initTime <- S.yieldM Time.getCurrentTime
  let
    t = adapt $ lastWait initTime es
    e = adapt $ e' initTime
    p = adapt $ p' initTime
    --nms :: s IO (NodeMetrics WattSeconds Watts)
  nms <- zipAsyncly $ nm <$> t <*> loss e <*> stored e <*> demand e <*> p <*> e
  return nms
  where
    e' t = powerAndEnergy t es & S.map (snd . snd)
    p' t = powerAndEnergy t es & S.map (fst . snd)
    es = nodeES n allEs
    --nm :: 
    nm t l s d p e = NodeMetrics t l s d p e

defaultES :: EnergyState
defaultES = defMessage
               & batteryVoltage .~ 0
               & gridVoltage .~ 0
               & batteryToLoadCurrent .~ 0
               & batteryToGridCurrent .~ 0
               & gridToBatteryCurrent .~ 0
               & solarInputCurrent .~ 0
               & dutyCycle .~ 0
