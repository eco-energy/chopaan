{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Chopaan.Kibbutz.Transactor where

import Prelude hiding (zip, zipWith)

import Chopaan.Kibbutz.Kibbutz (Kbtz(..), asMapStream)
import Chopaan.Comm.Comm (writeToPubQ, Dispatch(..), Address(..), PubQueue)
import Chopaan.Node.Node (pToE, Watts, WattSeconds, NodeS, Grid(..), NodeMetrics(..), Power(..))

import qualified Data.Time as Time
import qualified Data.Text as Text
import Data.Word

import qualified Proto.NodeMessageSchema.NodeMessages as NM
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as NM

import Lens.Micro

import Data.ProtoLens
import Data.Convertible
import Data.Convertible.Instances ()
import Data.ULID

import GHC.Generics (Generic)

import qualified Streamly.Prelude as S
import Streamly
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL

import qualified Data.Map.Strict as M
import Data.Key

import Chopaan.Utils.Time
import ConCat.Misc (R)

type T = Time.NominalDiffTime

mkETR :: R -> Int -> NM.PDirection -> Text.Text -> Time.UTCTime -> NM.EnergyTransactionRequest
mkETR power howLong dir uid stime = defMessage
         & NM.uuid .~ uid
         & NM.start .~ (utcToWord64 stime)
         & NM.powerInWatts .~ power
         & NM.durationInSeconds .~ (d' howLong)
         & NM.direction .~ dir
   where
     d' :: Int -> Word64
     d' = convert
     utcToWord64 :: Time.UTCTime -> Word64
     utcToWord64 = c'' . c'
       where
         c' :: Time.UTCTime -> Int
         c' = convert
         c'' :: Int -> Word64
         c'' = convert


data Role = Source | Sink deriving (Eq, Ord, Show, Generic)

newtype Stake = Stake { unStake :: (Role, Watts, Time.DiffTime) } deriving (Eq, Ord, Show, Generic)

newtype Tx n = Tx (M.Map n Stake) deriving (Eq, Ord, Show, Generic)

newtype TxState n = TxState (M.Map n (Role, TransactionStatus)) deriving (Eq, Ord, Show, Generic)

newtype NodeStates n = NodeStates (M.Map n NodeS) deriving (Eq, Ord, Show, Generic)

data TransactionStatus = TransactionStatus
  { energyDispatched :: WattSeconds
  , energyReceived :: WattSeconds
  , timeRemaining :: Time.DiffTime
  , energyRemaining :: WattSeconds
  , lossPerWattSecond :: WattSeconds
  , totalLoss :: WattSeconds
  , startLag :: Time.DiffTime
  , endLag :: Time.DiffTime
  } deriving (Eq, Ord, Show, Generic)

instance Semigroup TransactionStatus where
  tx <> tx' = TransactionStatus
              { energyDispatched = energyDispatched tx + energyDispatched tx'
              , energyReceived = energyReceived tx + energyReceived tx'
              , timeRemaining = min (timeRemaining tx) (timeRemaining tx')
              , energyRemaining = min (energyRemaining tx) (energyRemaining tx')
              , lossPerWattSecond =  avg (lossPerWattSecond tx) (lossPerWattSecond tx')
              , totalLoss = totalLoss tx + totalLoss tx'
              , startLag = max (startLag tx) (startLag tx')
              , endLag = max (endLag tx) (endLag tx)
              }
              where
                avg a b = (a + b) / 2

instance Monoid TransactionStatus where
  mempty = TransactionStatus
    { energyDispatched = 0
    , energyReceived = 0
    , timeRemaining = 0
    , energyRemaining = 0
    , lossPerWattSecond = 0
    , totalLoss = 0
    , startLag = 0
    , endLag = 0
    }



monitorTx :: (MonadAsync m, Address n, Ord n) => Tx n -> Kbtz SerialT m n NodeS -> SerialT m (TransactionStatus)
monitorTx tx k = S.postscan (transactionFold tx) s
  where s = NodeStates <$> asMapStream k

-- The state will just be carried across as a TransactionStatus
transactionFold :: forall m n. (Monad m, Address n, Ord n) => Tx n
  -> FL.Fold m (NodeStates n) (TransactionStatus)
transactionFold (Tx participants) = FL.Fold step start end
  where
    step :: TxState n -> NodeStates n -> m (TxState n)
    -- Zip instance for Map is defined as intersection with, so we don't need
    -- to filter the NodeStates.
    step (TxState ts) (NodeStates ns) = return . TxState $ zipWith updateTS ts ns 
    start :: m (TxState n)
    start = return . TxState $
      (\(Stake (px, w, t))-> (px, mempty{ timeRemaining = t
                          , energyRemaining = pToE (realToFrac t) w
                          , startLag = 0
                          }))
      <$> participants
    end :: TxState n -> m TransactionStatus
    end (TxState gt) = let
      gridTx = foldl (<>) mempty $ snd <$> gt
      loss = energyDispatched gridTx - energyReceived gridTx
      lossPerWS = loss / (energyDispatched gridTx)
      in return $ gridTx{totalLoss = loss, lossPerWattSecond = lossPerWS}
    updateTS :: (Role, TransactionStatus) -> NodeS -> (Role, TransactionStatus)
    updateTS (px, tx) NodeMetrics{..} = let
      nextTS = case px of
                 Source -> (mempty @TransactionStatus)
                           { energyDispatched = e + energyDispatched tx
                           , timeRemaining = timeRemaining tx - lastTimeDiff
                           , energyRemaining = energyRemaining tx - e
                           , startLag = if hasStarted px then startLag tx else (startLag tx + lastTimeDiff)
                           , endLag = if hasEnded px && shouldHaveEnded then 0 else lastTimeDiff
                           }
                 Sink -> (mempty @TransactionStatus)
                   { energyReceived = e + energyReceived tx
                   , timeRemaining = timeRemaining tx - lastTimeDiff
                   , energyRemaining = energyRemaining tx - e
                   , startLag = if hasStarted px then startLag tx else (startLag tx + lastTimeDiff)
                   , endLag = if not shouldHaveEnded then 0 else (if hasEnded px then endLag tx else endLag tx + lastTimeDiff)
                   }
      in (px, nextTS)
      where
        e :: WattSeconds
        e = pToE (realToFrac lastTimeDiff) (tOutP _powerT)
        hasStarted Source = (tOutP _powerT) >= eta
        hasStarted Sink = (tInP _powerT) >= eta
        hasEnded Source =  shouldHaveEnded && (tOutP _powerT) <= eta
        hasEnded Sink = shouldHaveEnded && (tInP _powerT) <= eta
        shouldHaveEnded = (energyRemaining tx - e) <= 0
        eta = 0.5
