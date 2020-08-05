{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Chopaan.Kibbutz.Transactor where

import Prelude hiding (zip, zipWith)
import Chopaan.Registry (writeToPubQ, PubQueue, Message, NodeT)
import Chopaan.Node (pToE, Watts, WattSeconds, NodeS, Grid(..), NodeMetrics(..), Power(..))
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


import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL

import qualified Data.Map.Strict as M
import Data.Key

type R = Double

class (Monad m, Show a) => Transactable m a where
  price :: a -> R
  execute :: a -> m a
  serialize :: (Message b) => a -> b


data Tx = Tx
  { voltage :: R
  , current :: R
  , resistance :: R
  , distance :: R
  , dt :: Time.NominalDiffTime
  } deriving (Eq, Ord, Show)

energyT :: Tx -> R
energyT Tx{..} = effEnergy
  where
    pAtV = (voltage**2 / resistance)
    iAtP = pAtV / voltage
    lossAtPI = (iAtP**2 * resistance)
    effPower = pAtV - lossAtPI
    effEnergy = effPower * (fromIntegral $ round dt)

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

newtype Transaction = Transaction
  { stakes :: [(NodeT, R, NM.EnergyTransactionRequest)]
  } deriving (Eq, Ord, Show, Generic)

data Stake = Stake
  { _stakingNode :: NodeT
  , _participating :: Bool
  , _power :: R
  , _duration :: Int
  } deriving (Eq, Ord, Show)


initStake :: NodeT -> Stake
initStake n = Stake n False 0 0

stakeEnergy :: Stake -> R
stakeEnergy Stake {..} = _power * (fromIntegral _duration)

validateStakeListForTx :: [Stake] -> Bool
validateStakeListForTx ss = energyBalance == 0 && powerBalance == 0
  where
    energyBalance = sum $ map stakeEnergy ss
    powerBalance = sum $ map _power ss

toTransaction :: [Stake] -> [NM.EnergyTransactionRequest] -> Transaction
toTransaction ss es = Transaction $ map (\(s, e) -> (_stakingNode s, stakeEnergy s, e)) $ zip ss es

toETR :: Stake -> (NodeT, (Text.Text -> Time.UTCTime -> NM.EnergyTransactionRequest))
toETR Stake {..} = (_stakingNode, msg)
  where
    msg = mkETR (abs _power) _duration dir
    dir = if (_power > 0) then NM.Outgoing else NM.Incoming

prepTx :: [Stake] -> ULID -> Time.UTCTime -> Time.NominalDiffTime -> ([(NodeT, NM.EnergyTransactionRequest)], Transaction)
prepTx sf ulid tNow leadTime = (txReqs, tx)
  where
  txId = (Text.pack . show) ulid
  startTime = Time.addUTCTime leadTime tNow
  txReqs = map (\(n, et) -> (n, et txId startTime)) etrs
  tx = toTransaction stakes (map snd txReqs)
  etrs = map toETR stakes
  stakes = filter (_participating) sf
  

data TransactorS = TransactorS
  { nodes_t :: [NodeT]
  , transactions :: [Transaction]
  , txForms :: [Stake]
  } deriving (Generic)

executeTransaction :: TransactorS -> PubQueue -> IO (TransactorS)
executeTransaction t@TransactorS{..} outQueue = if validateStakeListForTx txForms then exec else return t
  where
    exec = do
      ulid <- getULID
      startTime <- Time.getCurrentTime
      let
        (reqs, tx) = prepTx stakes ulid startTime (60 * 2 :: Time.NominalDiffTime)
      _ <- (mapM (uncurry $ writeToPubQ outQueue) reqs)
      return $ mkTransactor nodes_t $ tx:transactions
      where
        stakes = txForms
        
mkTransactor :: [NodeT] -> [Transaction] -> TransactorS
mkTransactor ns txs = TransactorS ns txs fs
  where
    fs = map initStake ns



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

data Participant = Source | Sink deriving (Eq, Ord, Show)

data Tx' = Tx'
  { stakez :: [Stake]
  , totalEnergy :: WattSeconds
  , totalTime :: Time.DiffTime
  , startTime :: Time.UTCTime
  , endTime :: Time.UTCTime
  }

type GridT = Grid NodeT (Participant, TransactionStatus)

-- The state will just be carried across as a TransactionStatus
transactionFold :: forall m. Monad m => M.Map NodeT (Participant, Watts, Time.DiffTime)
  -> FL.Fold m (Grid NodeT NodeS) (TransactionStatus)
transactionFold participants = FL.Fold step start end
  where
    step :: GridT -> Grid NodeT NodeS -> m (GridT)
    step (Grid ts) (Grid ma) = return . Grid $ zipWith updateTS ts ma 
    start :: m (GridT)
    start = return . Grid $
      (\(px, w, t)-> (px, mempty{ timeRemaining = t
                          , energyRemaining = pToE (realToFrac t) w
                          , startLag = 0
                          }))
      <$> participants
    end :: GridT -> m TransactionStatus
    end (Grid (gt)) = let
      gridTx = foldl (<>) mempty $ snd <$> gt
      loss = energyDispatched gridTx - energyReceived gridTx
      lossPerWS = loss / (energyDispatched gridTx)
      in return $ gridTx{totalLoss = loss, lossPerWattSecond = lossPerWS}
    updateTS :: (Participant, TransactionStatus) -> NodeS -> (Participant, TransactionStatus)
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
