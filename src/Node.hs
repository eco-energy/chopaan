{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Node where


import qualified Data.Time as Time
import Data.Time.Clock.POSIX
import qualified Data.Map.Strict as Map

-- Vis
import qualified Text.PrettyPrint.Tabulate as PPT
import GHC.Generics (Generic)
import Data.Data

import Proto.NodeMessages
import Proto.NodeMessages_Fields

import Lens.Micro

import Streamly
import Streamly.Prelude as S

----------------------------------------------------------------------------------
-- Metric Tracking

type WattSeconds = Double

type Watts = Double

type Demand = Double


data EnergyAudit = EnergyAudit
  { txIn :: WattSeconds
  , txOut :: WattSeconds
  , consumed :: WattSeconds
  , generated :: WattSeconds
  } deriving (Eq, Show, Ord, Generic, Data)

initEA :: EnergyAudit
initEA = EnergyAudit 0 0 0 0

instance PPT.Tabulate EnergyAudit PPT.ExpandWhenNested

-- Check associativity
instance Semigroup EnergyAudit where
  v1 <> v2 = EnergyAudit
    { txIn = txIn v1 + txIn v2
    , txOut = txOut v1 + txOut v2
    , consumed = consumed v1 + consumed v2
    , generated = generated v1 + generated v2
    }

instance Monoid EnergyAudit where
  mempty = initEA

-- Streams over T, one for each n.
stored :: (IsStream s, Monad m) => s m EnergyAudit -> s m WattSeconds
stored es = S.scanl' stored' 0 es
  where
    stored' :: WattSeconds -> EnergyAudit -> WattSeconds
    stored' s' (EnergyAudit{..}) = s' +
                                   (generated + ((withLossFrac cLoss) txIn))
                                   - ((withLossFrac dLoss txOut) + (withLossFrac cLoss) consumed)
    withLossFrac a = ((1 + a) *)
    cLoss = 0.01
    dLoss = 0.1

demand :: (IsStream s, Monad m) => s m EnergyAudit -> s m WattSeconds
demand es = S.map demand' es
  where
    demand' :: EnergyAudit -> WattSeconds
    demand' EnergyAudit{..} = (consumed - generated)

-- Imagine we're getting the energy audits of all the nodes for a certain window,
-- and that we have to calculate thesum txIn across all nodes
loss :: (IsStream s, Monad m) => s m EnergyAudit -> s m WattSeconds
loss es = (S.scanl' nLoss 0 es)
  where
    nLoss :: WattSeconds -> EnergyAudit -> WattSeconds
    nLoss l' (EnergyAudit {txOut, txIn}) = l' + (txOut - txIn)

lastWait :: (IsStream t, Monad m) => Time.UTCTime -> t m EnergyState -> t m Time.NominalDiffTime
lastWait t es = S.map snd $ S.scanl' sf (t, 0 :: Time.NominalDiffTime) es
  where
    sf :: (Time.UTCTime, Time.NominalDiffTime) -> EnergyState -> (Time.UTCTime, Time.NominalDiffTime)
    sf (ptime, _) e = (utcTNow e, Time.diffUTCTime (utcTNow e) ptime)


audit :: (IsStream t, Monad m) => Time.UTCTime -> t m EnergyState -> t m (Time.UTCTime, EnergyAudit)
audit t es' = S.scanl' audit' (t, initEA) es'
  where
    audit' :: (Time.UTCTime, EnergyAudit) -> EnergyState -> (Time.UTCTime, EnergyAudit)
    audit' (t', _) es = (utcTNow es, EnergyAudit txIn' txOut' cnsm' gen')
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
