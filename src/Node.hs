{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Node (
  -- functional export
  runNodeMonitor
  -- data constructors
  , EnergyState, NodeId(..), NodeS, NodeMetrics(..), Energy(..), Power(..), WattSeconds, Watts
  -- calculations exported for tests
  , stored, demand, loss, lastWait, energyStream, powerStream
  -- default builders
  , defaultES, defNodeS
  ) where
import qualified Data.Time as Time
import Data.Time.Clock.POSIX

import GHC.Generics (Generic)

import Proto.NodeMessages
import Proto.NodeMessages_Fields

import Lens.Micro

import Streamly
import qualified Streamly.Prelude as S

import Data.ProtoLens (defMessage)
import Data.ProtoLens.TextFormat

import Data.Hashable


----------------------------------------------------------------------------------
-- Metric Tracking

-- Our Scalars
type WattSeconds = Double

type Watts = Double

newtype NodeId a = NodeId { unNodeId :: a } deriving (Eq, Show, Ord, Generic)


instance (Hashable a) => Hashable (NodeId a) where
  hashWithSalt n (NodeId a) = hashWithSalt n a

-- Episodic Metrics

data Energy a = Energy
  { txIn :: !a
  , txOut :: !a
  , consumed :: !a
  , generated :: !a
  } deriving (Eq, Show, Ord, Generic, Functor)


instance Applicative Energy where
  pure v = Energy
    { txIn = v
    , txOut = v
    , consumed = v
    , generated = v
    }
  f <*> v = Energy
              { txIn = txIn f $ txIn v
              , txOut = txOut f $ txOut v
              , consumed = consumed f $ consumed v
              , generated = generated f $ generated v
              }

type EnergyBalance = Energy WattSeconds

initEA :: (Num a) => Energy a
initEA = Energy 0 0 0 0

-- Check associativity
instance (Num a) => Semigroup (Energy a) where
  v1 <> v2 = Energy
    { txIn = txIn v1 + txIn v2
    , txOut = txOut v1 + txOut v2
    , consumed = consumed v1 + consumed v2
    , generated = generated v1 + generated v2
    }

instance (Num a) => Monoid (Energy a) where
  mempty = initEA

data Power a = Power
  { gen :: !a
  , tIn :: !a
  , tOut :: !a
  , load :: !a }
  deriving (Eq, Ord, Show, Generic, Functor)


instance Applicative Power where
  pure v = Power
    { tIn = v
    , tOut = v
    , load = v
    , gen = v
    }
  f <*> v = Power
              { tIn = tIn f $ tIn v
              , tOut = tOut f $ tOut v
              , load = load f $ load v
              , gen = gen f $ gen v
              }


instance (Num a) => Semigroup (Power a) where
  p <> p' = (+) <$> p <*> p'

instance (Num a) => Monoid (Power a) where
  mempty = Power 0 0 0 0


data NodeMetrics e p = NodeMetrics
  { _lastW :: !Time.NominalDiffTime
  , _stored :: !e
  , _demand :: !e
  , _powerS :: !(Power p)
  , _energyS :: !(Energy e)
  , _sensors :: !EnergyState
  } deriving (Eq, Ord, Generic)
  

instance (Show e, Show p) => Show (NodeMetrics e p) where
  show NodeMetrics{..} = ("last connection: " <> show _lastW)
    <> sep <> ("current stored (Ws): " <> show _stored)
    <> sep <> ("current demand (Ws): " <> show _demand)
    <> sep <> ("current power:" <> sep <> show _powerS)
    <> sep <> ("current energy:" <> sep <> show _energyS)
    <> sep <> ("sensor readings:" <> sep <> (show (pprintMessage _sensors)))
    where sep = "\n"


defNodeS :: NodeS
defNodeS = NodeMetrics 0 0 0 mempty mempty defaultES

type NodeS = NodeMetrics WattSeconds Watts

data GridMetrics e p = GridMetrics
  { _uptime :: ! Time.NominalDiffTime
  , _loss :: !e
  }


runNodeMonitor :: (Eq a, Monad m, IsStream t, Applicative (t m)) => Time.UTCTime -> NodeId a -> t m (NodeId a, EnergyState) -> t m NodeS
runNodeMonitor initTime nodeId stream =
  let
    t = lastWait initTime nodeStream
    energyBalance = S.map snd $ energyStream initTime nodeStream
    power = powerStream nodeStream
    nms = NodeMetrics <$> t
      <*> stored energyBalance
      <*> demand energyBalance
      <*> power
      <*> energyBalance
      <*> nodeStream
  in nms
  where
    nodeStream = S.filter (\a-> fst a == nodeId) stream & S.map snd


utcTNow :: EnergyState -> Time.UTCTime
utcTNow es = posixSecondsToUTCTime $ fromIntegral $ es ^. cpuTime

defaultES :: EnergyState
defaultES = defMessage
               & batteryVoltage .~ 0
               & gridVoltage .~ 0
               & batteryToLoadCurrent .~ 0
               & batteryToGridCurrent .~ 0
               & gridToBatteryCurrent .~ 0
               & solarInputCurrent .~ 0
               & dutyCycle .~ 0
               & cpuTime .~ 0

stored :: (IsStream t, Monad m) => t m EnergyBalance -> t m WattSeconds
stored es = S.scanl' stored' 0 es
  where
    stored' :: WattSeconds -> EnergyBalance -> WattSeconds
    stored' s' (Energy{..}) = s' +
                              (generated + ((withLossFrac cLoss) * txIn))
                              - ((withLossFrac dLoss * txOut) + (withLossFrac cLoss) * consumed)
    withLossFrac a = (1 + a)
    cLoss = 0.01
    dLoss = 0.1

demand :: (IsStream t, Monad m) => t m EnergyBalance -> t m WattSeconds
demand es = S.map demand' es
  where
    demand' :: EnergyBalance -> WattSeconds
    demand' Energy{..} = (consumed - generated)

-- Imagine we're getting the energy audits of all the nodes for a certain window,
-- and that we have to calculate thesum txIn across all nodes
loss :: (IsStream t, Monad m) => t m EnergyBalance -> t m WattSeconds
loss es = (S.scanl' nLoss 0 es)
  where
    nLoss :: WattSeconds -> EnergyBalance -> WattSeconds
    nLoss l' (Energy {txOut, txIn}) = l' + (txOut - txIn)

lastWait :: (IsStream t, Monad m) => Time.UTCTime -> t m EnergyState -> t m Time.NominalDiffTime
lastWait t es = S.map snd $ S.scanl' sf (t, 0 :: Time.NominalDiffTime) es
  where
    sf :: (Time.UTCTime, Time.NominalDiffTime) -> EnergyState -> (Time.UTCTime, Time.NominalDiffTime)
    sf (ptime, _) e = (utcTNow e, Time.diffUTCTime (utcTNow e) ptime)


energyStream :: (IsStream t, Monad m) => Time.UTCTime -> t m EnergyState -> t m (Time.UTCTime, EnergyBalance)
energyStream t es = S.scanl' energyAtT (t, mempty) es
  where 
    energyAtT :: (Time.UTCTime, EnergyBalance) -> EnergyState -> (Time.UTCTime, EnergyBalance)
    energyAtT (prevT, prevEb) es' = (tNow, prevEb <> eb)
      where
        eb = Energy txIn' txOut' cnsm' gen'
        txIn' :: WattSeconds
        txIn' = integrate $ p batteryVoltage gridToBatteryCurrent 
        txOut' :: WattSeconds
        txOut' = integrate $ p batteryVoltage batteryToGridCurrent
        cnsm' :: WattSeconds
        cnsm' = integrate $ p batteryVoltage batteryToLoadCurrent
        gen' :: WattSeconds
        gen' = integrate $ p batteryVoltage solarInputCurrent
        p v i = es' ^. v * es' ^. i
        integrate p' = p' * delT
        delT = realToFrac $ Time.diffUTCTime tNow prevT
        tNow = utcTNow es'

powerStream :: (IsStream t, (Monad m)) => t m EnergyState -> t m (Power Watts)
powerStream = S.map powerAtT
  where
    powerAtT :: EnergyState -> Power Watts
    powerAtT es = Power txIn' txOut' cnsm' gen'
      where
        txIn' = p batteryVoltage gridToBatteryCurrent 
        txOut' = p batteryVoltage batteryToGridCurrent
        cnsm' = p batteryVoltage batteryToLoadCurrent
        gen' = p batteryVoltage solarInputCurrent
        p v i = es ^. v * es ^. i


--ns' :: (IsStream t) => [NodeT] -> t IO (NodeT Int, NodeS)
--ns' nodex = S.zipWith (,) (S.fromList $ P.cycle nodex) (S.repeat defNodeS{_energyS=es})
--  where es = Energy{txOut=10, txIn=10, consumed=10, generated=10}

