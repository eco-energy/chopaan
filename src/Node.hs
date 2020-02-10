{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Node (
  -- functional export
  runNodeMonitor
  -- data constructors
  , EnergyState, NodeId(..), NodeS, NodeMetrics(..), EnergyBalance(..)
  -- calculations exported for tests
  , stored, demand, loss, lastWait, energyStream, powerStream
  -- default builders
  , defaultES, defNodeS
  ) where
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

import Data.Hashable
----------------------------------------------------------------------------------
-- Metric Tracking

-- Our Scalars
type WattSeconds = Double

type Watts = Double

type Demand = Double

newtype NodeId a = NodeId { unNodeId :: a } deriving (Eq, Show, Ord, Data, Generic)

instance (Show a) => PPT.CellValueFormatter (NodeId a)

instance (Hashable a) => Hashable (NodeId a) where
  hashWithSalt n (NodeId a) = hashWithSalt n a

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
  , _sensors :: EnergyState
  } deriving (Eq, Ord, Show, Generic)



defNodeS :: NodeS
defNodeS = NodeMetrics 0 0 0 0 mempty mempty defaultES

type NodeS = NodeMetrics WattSeconds Watts


runNodeMonitor :: (Eq a, Monad m, IsStream t, Applicative (t m)) => Time.UTCTime -> NodeId a -> t m (NodeId a, EnergyState) -> t m NodeS
runNodeMonitor initTime nodeId stream =
  let
    t = lastWait initTime nodeStream
    energyBalance = S.map snd $ energyStream initTime nodeStream
    power = powerStream nodeStream
    nms = NodeMetrics <$> t
      <*> loss energyBalance
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
    stored' s' (EnergyBalance{..}) = s' +
                                     (generated + ((withLossFrac cLoss) * txIn))
                                     - ((withLossFrac dLoss * txOut) + (withLossFrac cLoss) * consumed)
    withLossFrac a = (1 + a)
    cLoss = 0.01
    dLoss = 0.1

demand :: (IsStream t, Monad m) => t m EnergyBalance -> t m WattSeconds
demand es = S.map demand' es
  where
    demand' :: EnergyBalance -> WattSeconds
    demand' EnergyBalance{..} = (consumed - generated)

-- Imagine we're getting the energy audits of all the nodes for a certain window,
-- and that we have to calculate thesum txIn across all nodes
loss :: (IsStream t, Monad m) => t m EnergyBalance -> t m WattSeconds
loss es = (S.scanl' nLoss 0 es)
  where
    nLoss :: WattSeconds -> EnergyBalance -> WattSeconds
    nLoss l' (EnergyBalance {txOut, txIn}) = l' + (txOut - txIn)

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
        eb = EnergyBalance txIn' txOut' cnsm' gen'
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
