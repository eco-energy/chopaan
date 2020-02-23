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
  -- scans
  gridS, nodeS, energyS, powerS, timeS
  -- folds
  , energyFold, power, time
  -- data constructors
  , EnergyState, NodeId(..), NodeS, NodeMetrics(..), Energy(..), Power(..), WattSeconds, Watts
  -- default builders
  , zeroMsg, defNodeS
  ) where

import qualified Data.Time as Time
import Data.Time.Clock.POSIX

import GHC.Generics (Generic)

import Proto.NodeMessages
import Proto.NodeMessages_Fields hiding (time)

import Lens.Micro

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL

import Data.ProtoLens (defMessage)
import Data.ProtoLens.TextFormat

import Data.Hashable
import Subscriber
import qualified Data.Map.Strict as Map
import Data.Function ((&))


import ConCat.Free.Affine (Affine(..))
import qualified ConCat.Free.Affine as Aff
import qualified ConCat.GradientDescent as GD
import qualified ConCat.Scan as Scan
import qualified ConCat.Free.LinearRow as LR
import qualified ConCat.Free.VectorSpace as VS
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

--instance (Num a) => VS.V R (Power a) where

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
defNodeS = NodeMetrics Nothing mempty mempty zeroMsg

type NodeS = NodeMetrics WattSeconds Watts

type Timestamp = (Time.UTCTime, Time.NominalDiffTime)







{-
runNodeMonitor :: (IsStream t, MoncadAsync m) => Time.UTCTime -> ZipSerialM m (EnergyState) -> t m (NodeMetrics WattSeconds Watts)
runNodeMonitor initTime stream = zipSerially $ NodeMetrics <$> t <*> p <*> en <*> stream
  where
    t = S.map (\e -> Just e) $ timeStream stream
    dt = (timeDiff initTime $ timeStream stream)
    p = powerStream stream
    en = energyStream p dt
--}



{----------------------------------------------------------------------------------------------------


Folds of type FL.Fold, as functions to the instantatenous values of the system over an indexed set.
                      :: forall s. Fold (s -> a -> m s) (m s) (s -> m b)


-----------------------------------------------------------------------------------------------------}

time :: forall m. Monad m => Time.UTCTime -> FL.Fold m (EnergyState) Timestamp
time startT = FL.Fold step' begin' done'
  where
    step' :: (Timestamp -> EnergyState -> m Timestamp)
    step' (!prev, _) cur = pure (tn, Time.diffUTCTime tn prev)
      where
        tn = utcTimeNow cur
    begin' :: m Timestamp
    begin' = pure (startT, 0)
    done' :: Timestamp -> m Timestamp
    done' = pure


-- (s -> a -> m s) (m s) (s -> m b)
power :: forall m a. (Monad m) => FL.Fold m EnergyState (Power Watts)
power = FL.Fold powerAtT (pure $ mempty) return
  where
    powerAtT _ es = pure $ Power
                    { tIn = txIn'
                    , tOut = txOut'
                    , load = cnsm'
                    , gen = gen' }
      where
        txIn' = p batteryVoltage gridToBatteryCurrent
        txOut' = p batteryVoltage batteryToGridCurrent
        cnsm' = p batteryVoltage batteryToLoadCurrent
        gen' = p batteryVoltage solarInputCurrent
        p :: Getting Double EnergyState Double -> Getting Double EnergyState Double -> Watts
        p v i = es ^. v * es ^. i


energyFold :: forall m. (Monad m) => Time.UTCTime -> FL.Fold m (EnergyState) (Energy WattSeconds)
energyFold startT = eAtT <$> power <*> td
  where
    --pt :: FL.Fold m (EnergyState) (Power Watts, Timestamp)
    --pt = ((,) <$> power <*> td)
    td = time startT
    --eFold :: FL.Fold m (Power Watts, Timestamp) EnergyBalance
    --eFold = FL.Fold eAtT (pure mempty) (pure)
    eAtT :: (Power Watts) ->  Timestamp -> (Energy WattSeconds)
    eAtT p (_, t) = Energy { txIn = (pToE t tIn)
                           , txOut = (pToE t tOut)
                           , consumed = (pToE t load)
                           , generated = (pToE t gen)}
      where
        Power{..} = p
    --pToE :: (Num b) => Time.NominalDiffTime -> b -> a
    pToE t p' = p' * (realToFrac t)


nodeMonitor :: forall m. (Monad m) => Time.UTCTime -> FL.Fold m (EnergyState) (NodeMetrics Watts WattSeconds) 
nodeMonitor startT = NodeMetrics <$> ((Just . fst) <$> tn) <*> power <*> en <*> sensors 
  where
    tn :: FL.Fold m (EnergyState) Timestamp
    tn = time startT
    en :: FL.Fold m (EnergyState) (Energy WattSeconds)
    en =  energyFold startT
    sensors :: FL.Fold m (EnergyState) (EnergyState)
    sensors = FL.Fold (\_ nes -> pure nes) (pure zeroMsg) (pure) 



runNodeMonitor :: forall m t. (MonadAsync m, IsStream t) => Time.UTCTime -> t m EnergyState -> t m NodeS
runNodeMonitor initTime = S.scan (nodeMonitor initTime)

{--------------------------------------------------------------------------------------------------------------

                                          Streams of Folds
---------------------------------------------------------------------------------------------------------------}


{--
slice :: (IsStream t, Monad m) => Int -> Int -> t m a -> t m a
slice i j = (S.take (j - i)) . (S.drop i)

eval :: (IsStream t, Monad m) => (a -> b) -> (FL.Fold m a b) -> Int -> Int -> t m a -> t m b
eval e f start end = S.map e $ S.scan f $ slice start end 


evalAll :: (a -> b) -> FL.Fold m a b -> t m a -> t m b
evalAll e f x =  eval e f (0) (S.length x) $ x
--}

--type Stream a = t m a

energyS :: (MonadAsync m, IsStream t) => Time.UTCTime -> t m EnergyState -> t m (Energy WattSeconds)
energyS = S.postscan . energyFold

timeS :: (MonadAsync m, IsStream t) => Time.UTCTime -> t m EnergyState -> t m Timestamp
timeS = S.postscan . time


powerS :: (MonadAsync m, IsStream t) => t m EnergyState -> t m (Power Watts)
powerS = S.postscan power

nodeS :: (MonadAsync m, IsStream t) => Time.UTCTime -> t m EnergyState -> t m NodeS
nodeS = S.postscan . nodeMonitor


gridS :: forall t m n . (IsStream t, MonadAsync m, Ord n) => Time.UTCTime -> [n] -> t m (n, EnergyState) -> t m (Map.Map n (NodeS))
gridS startT ns ss = S.postscan gridMap ss
  where
    gridMap = FL.demux nodeMap
      where
        nodeMap = Map.fromList $ zip ns $ repeat (nodeMonitor startT) 




{---------------------------------------------------------------------------------------------------------------------

                                          Helper Functions
---------------------------------------------------------------------------------------------------------------------}


utcTimeNow :: EnergyState -> Time.UTCTime
utcTimeNow es = posixSecondsToUTCTime $ fromIntegral $ es ^. cpuTime

zeroMsg :: EnergyState
zeroMsg = defMessage
               & batteryVoltage .~ 0
               & gridVoltage .~ 0
               & batteryToLoadCurrent .~ 0
               & batteryToGridCurrent .~ 0
               & gridToBatteryCurrent .~ 0
               & solarInputCurrent .~ 0
               & dutyCycle .~ 0
               & cpuTime .~ 0

hamiltonianLoss = undefined
