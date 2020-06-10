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
module Transactor where

import Registry (writeToPubQ, PubQueue, Message, NodeT, KibbutzEvents)
import Node (pToE, Watts, WattSeconds, NodeId(..), NodeS, Grid(..), NodeMetrics(..), Power(..), Energy(..), toWattSeconds)
import qualified Data.Time as Time
import qualified Data.Text as Text
import Data.Word

import qualified Proto.NodeMessages as NM
import qualified Proto.NodeMessages_Fields as NM

import Lens.Micro
import Lens.Micro.TH (makeLenses)


import Data.ProtoLens
import Data.Convertible
import Data.Convertible.Instances ()
import Data.ULID

import GHC.Generics (Generic)


-- Brick
import qualified Brick.Forms as F
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import Brick.Widgets.Core (strWrap, fill, padBottom, (<+>), vLimit, hLimit)


import qualified Data.Vector as Vec
import UI.Types (TXFormField(..), KibbutzUI(..))

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL

import qualified Data.Set as St
import qualified Data.Map.Strict as M


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


makeLenses ''Stake

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
  , txForms :: StakeList
  } deriving (Generic)

executeTransaction :: TransactorS -> PubQueue -> IO (TransactorS)
executeTransaction t@TransactorS{..} outQueue = if validateStakeListForTx (unStakeList txForms) then exec else return t
  where
    exec = do
      ulid <- getULID
      startTime <- Time.getCurrentTime
      let
        (reqs, tx) = prepTx stakes ulid startTime (60 * 2 :: Time.NominalDiffTime)
      _ <- (mapM (uncurry $ writeToPubQ outQueue) reqs)
      return $ mkTransactor nodes_t $ tx:transactions
      where
        stakes = unStakeList txForms
        
mkTransactor :: [NodeT] -> [Transaction] -> TransactorS
mkTransactor ns txs = TransactorS ns txs fs
  where
    fs = stakeList $ mkTForms ns $ map initStake ns


type StakeForm = F.Form Stake KibbutzEvents KibbutzUI

type StakeList = L.List KibbutzUI StakeForm


stakeList :: [StakeForm] -> StakeList
stakeList xs = L.list TxListUI (Vec.fromList xs) 1 

unStakeList :: StakeList -> [Stake]
unStakeList s =  F.formState <$> (Vec.toList . L.listElements $ s)

initStakeList :: StakeList
initStakeList = stakeList []

addStake :: StakeList -> StakeForm -> StakeList
addStake xs x = L.listInsert 0 x xs


stakeForm :: Int -> NodeT -> Stake -> StakeForm
stakeForm i n =
    let
      selQ = "Household?"
      hname = (unNodeId n)
      label s w = padBottom (T.Pad 1) $ (vLimit 2 $ hLimit 25 $ strWrap s <+> fill ' ') <+> w
    in F.newForm [ label selQ F.@@= F.checkboxField participating (TxFormUI (ParticipatingField i)) hname   
                 , label "Power" F.@@= F.editShowableField power (TxFormUI (PowerField i))
                 , label "Duration" F.@@= F.editShowableField duration (TxFormUI (DurationField i))
                 ]

mkTForms :: [NodeT] -> [Stake] -> [StakeForm]
mkTForms ns stakes = map (uncurry3 stakeForm) $ zip3 ids ns stakes
  where
    uncurry3 f (a, b, c) = f a b c
    ids = [1,2..]


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
    step (Grid ts) (Grid ma) = return . Grid $ updateTS <$> ts <*> ma 
    start :: m (GridT)
    start = return . Grid $
      (\(px, w, t)-> (px, mempty{ timeRemaining = t
                          , energyRemaining = pToE w (fromIntegral t)
                          , startLag = 0
                          }))
      <$> participants
    end :: GridT -> m TransactionStatus
    end (Grid gt) = let
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
                           , startLag = if hasStarted then 0 else lastTimeDiff
                           , endLag = if hasEnded && shouldHaveEnded then 0 else lastTimeDiff
                           }
                 Sink -> (mempty @TransactionStatus)
                   { energyReceived = e + energyReceived tx
                   , timeRemaining = timeRemaining tx - lastTimeDiff
                   , energyRemaining = energyRemaining tx - e
                   , startLag = if hasStarted then 0 else lastTimeDiff
                   , endLag = if hasEnded && shouldHaveEnded then 0 else lastTimeDiff
                   }
      in (px, nextTS)
      where
        e :: WattSeconds
        e = pToE (tOutP _powerT) (fromIntegral lastTimeDiff)
        hasStarted = (tOutP _powerT) > 0.5
        hasEnded =  (tOutP _powerT) <= 0 && e > 0.5
        shouldHaveEnded = (energyRemaining tx - e) <= 0
