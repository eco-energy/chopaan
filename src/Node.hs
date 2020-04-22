{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
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
  , energyFold, powerFold, timeFold
  -- data constructors
  , EnergyState, NodeId(..), NodeS, NodeMetrics(..), Energy(..), Power(..), WattSeconds, Watts
  -- default builders
  , zeroMsg, defNodeS
  , nmFilter
  , writeCSVRecords
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
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Csv as Csv
--import qualified Streamly.Internal.FileSystem.File as FH
import qualified Streamly.External.ByteString as SBS
import qualified Streamly.Memory.Array as A

import Data.ProtoLens (defMessage)
import Data.ProtoLens.TextFormat

import Data.Hashable
import qualified Data.Map.Strict as Map
import Data.Function ((&))
import Data.Maybe (isJust)

import Data.Csv
import qualified Data.Vector as Vec (fromList)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 (pack)
import Data.Word
import System.IO
import System.Directory
import qualified Data.HashMap.Strict as HM

----------------------------------------------------------------------------------
-- Metric Tracking

-- Our Scalars

type WattSeconds = Double

type Watts = Double

newtype NodeId a = NodeId { unNodeId :: a } deriving (Eq, Show, Ord, Generic)

instance (Hashable a) => Hashable (NodeId a) where
  hashWithSalt n (NodeId a) = hashWithSalt n a


instance (ToField a) => ToField (NodeId a) where
  toField (NodeId a) = toField a 

-- Episodic Metrics

data Energy a = Energy
  { txIn :: !a
  , txOut :: !a
  , consumed :: !a
  , generated :: !a
  } deriving (Eq, Show, Ord, Generic, Functor)

instance (ToField a) => ToNamedRecord (Energy a)

instance DefaultOrdered (Energy a)

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
  { genP :: !a
  , tInP :: !a
  , tOutP :: !a
  , loadP :: !a }
  deriving (Eq, Ord, Show, Generic, Functor)

instance (ToField a) => ToNamedRecord (Power a)

instance DefaultOrdered (Power a)

instance Applicative Power where
  pure v = Power
    { tInP = v
    , tOutP = v
    , loadP = v
    , genP = v
    }
  f <*> v = Power
              { tInP = tInP f $ tInP v
              , tOutP = tOutP f $ tOutP v
              , loadP = loadP f $ loadP v
              , genP = genP f $ genP v
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
  , _powerT :: !(Power p)
  , _energyT :: !(Energy e)
  , _sensorsT :: !EnergyState
  } deriving (Eq, Ord, Generic)


--deriving instance Generic EnergyState
instance ToNamedRecord EnergyState where
  toNamedRecord es = HM.fromList $
                zip names $
                map (pack . show) $
                es ^.. ( batteryVoltage
                         <> gridVoltage
                         <> batteryToLoadCurrent
                         <> batteryToGridCurrent
                         <> gridToBatteryCurrent
                         <> solarInputCurrent
                         <> dutyCycle
                       )
                where
                  names = ["batteryV",
                           "gridV",
                           "battery2LoadC",
                           "battery2GridC",
                           "grid2BatteryC",
                           "solarC",
                           "dutyC"]


instance DefaultOrdered EnergyState where
  headerOrder _ = Vec.fromList $ names
    where
      names = ["batteryV",
                "gridV",
                "battery2LoadC",
                "battery2GridC",
                "grid2BatteryC",
                "solarC",
                "dutyC"]

instance ToField Time.UTCTime where
  toField t = pack (show t)

instance (ToField e, ToField p) => ToNamedRecord (NodeMetrics e p) where
  toNamedRecord (NodeMetrics {..}) = foldl (HM.union) (HM.fromList [("time", toField _time)])
    [toNamedRecord _powerT,
      toNamedRecord _energyT,
      toNamedRecord _sensorsT
    ]

instance (Show e, Show p) => Show (NodeMetrics e p) where
  show NodeMetrics{..} = ("last connection: " <> show _time)
    -- <> sep <> ("current stored (Ws): " <> show _stored)
    -- <> sep <> ("current demand (Ws): " <> show _demand)
    <> sep <> ("current power:" <> sep <> show _powerT)
    <> sep <> ("current energy:" <> sep <> show _energyT)
    <> sep <> ("sensor readings:" <> sep <> (show (pprintMessage _sensorsT)))
    where sep = "\n"


defNodeS :: NodeS
defNodeS = NodeMetrics Nothing mempty mempty zeroMsg

nmFilter :: (NodeId a) -> NodeS -> Bool
nmFilter _ = isJust . _time 

type NodeS = NodeMetrics WattSeconds Watts

instance DefaultOrdered (NodeMetrics p e)

type Timestamp = (Maybe Time.UTCTime, Time.NominalDiffTime)


{----------------------------------------------------------------------------------------------------


Folds of type FL.Fold, as functions to the instantatenous values of the system over an indexed set.
                      :: forall s. Fold (s -> a -> m s) (m s) (s -> m b)


-----------------------------------------------------------------------------------------------------}

timeFold :: forall m. Monad m => FL.Fold m (EnergyState) Timestamp
timeFold = FL.Fold step' begin' done'
  where
    step' :: (Timestamp -> EnergyState -> m Timestamp)
    step' (Nothing, _) cur = pure (Just tn, Time.diffUTCTime tn tn)
      where
        tn = utcTimeNow cur
    step' ((Just !prev), _) cur = pure (Just tn, Time.diffUTCTime tn prev)
      where
        tn = utcTimeNow cur
    begin' :: m Timestamp
    begin' = pure (Nothing, 0)
    done' :: Timestamp -> m Timestamp
    done' = pure

-- (s -> a -> m s) (m s) (s -> m b)
powerFold :: forall m. (Monad m) => FL.Fold m EnergyState (Power Watts)
powerFold = FL.Fold (\_ b-> pure $ power b) (pure $ mempty) return 

energyFold :: forall m. (Monad m) => FL.Fold m (EnergyState) (Energy WattSeconds)
energyFold = (FL.Fold step begin end)
  where
    -- forall s. Fold (s -> a -> m s) (m s) (s -> m b)
    step :: (Energy WattSeconds, Maybe Time.UTCTime) -> EnergyState -> m (Energy WattSeconds, Maybe Time.UTCTime)
    step (esPrev, (Just tPrev)) cur = pure $ ((esPrev <> (eAtT (power cur) (Just tn, Time.diffUTCTime tn tPrev))), Just tn)
      where
        tn = utcTimeNow cur
    step (esPrev, Nothing) cur = pure $ ((esPrev <> (eAtT (power cur) (Just tn, Time.diffUTCTime tn tn))), Just tn)
      where
        tn = utcTimeNow cur
    begin :: m (Energy WattSeconds, Maybe Time.UTCTime)
    begin = pure $ (mempty, Nothing)
    end :: (Energy WattSeconds, Maybe Time.UTCTime) -> m (Energy WattSeconds)
    end = pure . fst
    eAtT :: Power Watts -> Timestamp -> (Energy WattSeconds)
    eAtT p (_, t) = Energy { txIn = (pToE t tInP)
                           , txOut = (pToE t tOutP)
                           , consumed = (pToE t loadP)
                           , generated = (pToE t genP)}
      where
        Power{..} = p
    pToE t p' = p' * (realToFrac t)

nodeMonitor :: forall m. (Monad m) => FL.Fold m (EnergyState) (NodeMetrics Watts WattSeconds) 
nodeMonitor = NodeMetrics <$> ((fst) <$> tn) <*> powerFold <*> en <*> sensors 
  where
    tn :: FL.Fold m (EnergyState) Timestamp
    tn = timeFold
    en :: FL.Fold m (EnergyState) (Energy WattSeconds)
    en =  energyFold
    sensors :: FL.Fold m (EnergyState) (EnergyState)
    sensors = FL.Fold (\_ nes -> pure nes) (pure zeroMsg) (pure) 


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

energyS :: (MonadAsync m, IsStream t) => t m EnergyState -> t m (Energy WattSeconds)
energyS = S.postscan energyFold

timeS :: (MonadAsync m, IsStream t) => t m EnergyState -> t m Timestamp
timeS = S.postscan timeFold

powerS :: (MonadAsync m, IsStream t) => t m EnergyState -> t m (Power Watts)
powerS = S.postscan powerFold

nodeS :: (MonadAsync m, IsStream t) => t m EnergyState -> t m NodeS
nodeS = S.postscan nodeMonitor

gridS :: forall t m n . (IsStream t, MonadAsync m, Ord n) => [n] -> t m (n, EnergyState) -> t m (Map.Map n (NodeS))
gridS ns ss = S.postscan gridMap $ ss
  where
    gridMap = FL.demux nodeMap
      where
        nodeMap = Map.fromList $ zip ns $ repeat nodeMonitor 


newtype TaggedNode n = TaggedNode (n, NodeS) deriving (Generic)

instance (ToField n) => ToNamedRecord (TaggedNode n) where
  toNamedRecord (TaggedNode (n, ns)) = (HM.fromList [("NodeId", toField n)]) <> toNamedRecord ns

instance DefaultOrdered (TaggedNode n) where
  headerOrder _ = (Vec.fromList $ ["NodeId", "time"])
                  <> (headerOrder (undefined :: EnergyState))
                  <> (headerOrder (undefined :: Power Watts))
                  <> (headerOrder (undefined :: Energy WattSeconds))
--instance DefaultOrdered (TaggedNode n) where

--TODO: Generalize This


writeCSVRecords :: forall n. (ToField n)
  => FilePath
  -> Map.Map n (NodeS)
  ->  IO ()
writeCSVRecords fp gs = do
  fE <- doesFileExist fp
  let
    opts = if fE then contOpts else initOpts 
  withFile fp AppendMode $ (\ho ->do
      (BSL.hPut ho) $ encodeDefaultOrderedByNameWith opts as)
  where
    contOpts = defaultEncodeOptions {
      encUseCrLf = True,
      encIncludeHeader = False
    }
    initOpts = contOpts { encIncludeHeader = True }
    --checkFileExists = isFile fp
    as = atT gs
    atT :: Map.Map n (NodeS) -> [TaggedNode n]
    atT nmap = TaggedNode <$> Map.toList nmap


{---------------------------------------------------------------------------------------------------------------------

                                          Helper Functions
---------------------------------------------------------------------------------------------------------------------}


power :: EnergyState -> Power Watts
power es = Power
           { tInP = txIn'
           , tOutP = txOut'
           , loadP = cnsm'
           , genP = genP' }
  where
    txIn' = p batteryVoltage gridToBatteryCurrent
    txOut' = p batteryVoltage batteryToGridCurrent
    cnsm' = p batteryVoltage batteryToLoadCurrent
    genP' = p batteryVoltage solarInputCurrent
    p :: Getting Double EnergyState Double -> Getting Double EnergyState Double -> Watts
    p v i = es ^. v * es ^. i


utcTimeNow :: EnergyState -> Time.UTCTime
utcTimeNow es = posixSecondsToUTCTime $ ((fromIntegral $ (es ^. cpuTime)) / 1000)

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
