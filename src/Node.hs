{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Node (
  -- functional export
  runNodeMonitor
  -- data constructors
  , EnergyState, NodeId(..), NodeS, NodeMetrics(..), Energy(..), Power(..), WattSeconds, Watts
  -- calculations exported for tests
  , energyStream, powerStream, timeDiff, timeStream
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
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL

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
  { _time :: !(Maybe Time.UTCTime)
  -- , _stored :: !e
  -- , _demand :: !e
  , _powerS :: !(Power p)
  , _energyS :: !(Energy e)
  , _sensors :: !EnergyState
  } deriving (Eq, Ord, Generic)
  

instance (Show e, Show p) => Show (NodeMetrics e p) where
  show NodeMetrics{..} = ("last connection: " <> show _time)
    -- <> sep <> ("current stored (Ws): " <> show _stored)
    -- <> sep <> ("current demand (Ws): " <> show _demand)
    <> sep <> ("current power:" <> sep <> show _powerS)
    <> sep <> ("current energy:" <> sep <> show _energyS)
    <> sep <> ("sensor readings:" <> sep <> (show (pprintMessage _sensors)))
    where sep = "\n"


defNodeS :: NodeS
defNodeS = NodeMetrics Nothing mempty mempty defaultES

type NodeS = NodeMetrics WattSeconds Watts

data GridMetrics e p = GridMetrics
  { _uptime :: ! Time.NominalDiffTime
  , _loss :: !e
  }


--runNodeMonitor :: (Eq a, Monad m, IsStream t, Applicative (t m)) => Time.UTCTime -> NodeId a -> t m (NodeId a, EnergyState) -> ZipSerialM m NodeS
runNodeMonitor :: (IsStream t, Eq a, Monad m) => Time.UTCTime -> a -> ZipSerialM m (a, EnergyState) -> t m (NodeMetrics WattSeconds Watts)
runNodeMonitor initTime nodeId stream = zipSerially $ NodeMetrics <$> t <*> p <*> en <*> thisNode
  where
    t = S.map (\e -> Just e) $ timeStream thisNode
    dt = (timeDiff initTime $ timeStream thisNode)
    p = powerStream thisNode
    en = energyStream p dt
    thisNode = S.map (snd) . S.filter (\e -> fst e == nodeId) $ stream

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


energyStream :: (IsStream t, Monad m) => t m (Power Watts) -> t m (Time.NominalDiffTime) -> t m (Energy WattSeconds)
energyStream p dt = S.postscan (FL.mconcat) eAtT
  where
    eAtT = S.zipWith (\Power{..} t-> Energy { txIn = (pToE t tIn)
                                                , txOut = (pToE t tOut)
                                                , consumed = (pToE t load)
                                                , generated = (pToE t gen)}) p dt
    pToE :: Time.NominalDiffTime -> Watts ->  WattSeconds
    pToE t p' = p' * (realToFrac t)
    
-- FL.Fold :: forall s. Fold (s -> a -> m s) (m s) (s -> m b)
timeDiff :: forall m t . (IsStream t, (Monad m)) => Time.UTCTime -> t m (Time.UTCTime) -> t m (Time.NominalDiffTime)
timeDiff st = S.scan (FL.Fold step' begin' done')
  where
    step' :: ((Time.UTCTime, Time.NominalDiffTime) -> Time.UTCTime -> m (Time.UTCTime, Time.NominalDiffTime))
    step' (!prev, _) cur = pure (cur, Time.diffUTCTime cur prev)
    begin' :: m (Time.UTCTime, Time.NominalDiffTime)
    begin' = return (st, 0)
    done' :: (Time.UTCTime, Time.NominalDiffTime) -> m Time.NominalDiffTime
    done' = pure . snd

timeStream :: (IsStream t, (Monad m)) => t m EnergyState -> t m (Time.UTCTime)
timeStream = S.map utcTNow

powerStream :: (IsStream t, (Monad m)) => t m EnergyState -> t m (Power Watts)
powerStream = S.map powerAtT
  where
    powerAtT :: EnergyState -> Power Watts
    powerAtT es = Power { tIn = txIn'
                        , tOut = txOut'
                        , load = cnsm'
                        , gen = gen'}
      where
        txIn' = p batteryVoltage gridToBatteryCurrent
        txOut' = p batteryVoltage batteryToGridCurrent
        cnsm' = p batteryVoltage batteryToLoadCurrent
        gen' = p batteryVoltage solarInputCurrent
        p v i = es ^. v * es ^. i




--ns' :: (IsStream t) => [NodeT] -> t IO (NodeT Int, NodeS)
--ns' nodex = S.zipWith (,) (S.fromList $ P.cycle nodex) (S.repeat defNodeS{_energyS=es})
--  where es = Energy{txOut=10, txIn=10, consumed=10, generated=10}

