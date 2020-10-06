{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Chopaan.Kibbutz.Transactor where

import Prelude hiding (zip, zipWith)

import Control.Monad.IO.Class

import Chopaan.Kibbutz.Kibbutz (Kbtz(..), asMapStream)
import Chopaan.Comm.Comm (writeToPubQ, Dispatch(..), Address(..), PubQueue)
import Chopaan.Node.Node (pToE, toWattSeconds, toWatts, fromWattSeconds, fromWatts, Watts, WattSeconds, NodeS, Grid(..), NodeMetrics(..), Power(..), Battery(..))

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
import qualified Data.List as L

import Chopaan.Utils.Time
import Chopaan.Kibbutz.LinOpt
import ConCat.Misc (R)

import Data.SBV

type T = Time.NominalDiffTime

mkETR :: Watts -> Time.DiffTime -> NM.PDirection -> Text.Text -> Time.UTCTime -> NM.EnergyTransactionRequest
mkETR power howLong dir uid stime = defMessage
         & NM.uuid .~ uid
         & NM.start .~ (utcToWord64 stime)
         & NM.powerInWatts .~ (fromWatts power)
         & NM.durationInSeconds .~ (timeToWord howLong)
         & NM.direction .~ dir
  where
    timeToWord :: Time.DiffTime -> Word64
    timeToWord = c'' . (round @Time.DiffTime @Int)
    utcToWord64 :: Time.UTCTime -> Word64
    utcToWord64 = c'' . c'
      where
        c' :: Time.UTCTime -> Int
        c' = convert
    c'' :: Int -> Word64
    c'' = convert



fromStake :: Time.UTCTime -> Stake -> NM.EnergyTransactionRequest
fromStake now (Stake (role, watts, duration)) = mkETR watts duration (toPDir role) "" now
  where
    toPDir Source = NM.Outgoing
    toPDir Sink = NM.Incoming


data Role = Source | Sink deriving (Eq, Ord, Show, Generic)

newtype Stake = Stake { unStake :: (Role, Watts, Time.DiffTime) } deriving (Eq, Ord, Show, Generic)

newtype Tx n = Tx (M.Map n Stake) deriving (Eq, Ord, Show, Generic)

instance (Ord n) => Semigroup (Tx n) where
  (Tx m) <> (Tx m') = Tx (m <> m')

instance (Ord n) => Monoid (Tx n) where
  mempty = Tx mempty

newtype TxState n = TxState (M.Map n (Role, TransactionStatus)) deriving (Eq, Ord, Show, Generic)

instance (Ord n) => Semigroup (TxState n) where
  (TxState m) <> (TxState m') = TxState (m <> m')

instance (Ord n) => Monoid (TxState n) where
  mempty = TxState mempty

newtype NodeStates n = NodeStates (M.Map n NodeS) deriving (Eq, Ord, Show, Generic)

instance (Ord n) => Semigroup (NodeStates n) where
  (NodeStates m) <> (NodeStates m') = NodeStates (m <> m')

instance (Ord n) => Monoid (NodeStates n) where
  mempty = NodeStates mempty


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

toNodeStates :: (MonadAsync m, Address n, Ord n) => Kbtz SerialT m n NodeS -> SerialT m (NodeStates n)
toNodeStates k = NodeStates <$> asMapStream k


planTx :: (MonadAsync m, Address n, Ord n) => [n] -> Kbtz SerialT m n NodeS -> SerialT m (Tx n)
planTx ns k = S.postscan (transactionPlanner ns) $ toNodeStates k

monitorTx :: (MonadAsync m, Address n, Ord n) => Tx n -> Kbtz SerialT m n NodeS -> SerialT m (TransactionStatus)
monitorTx tx k = S.postscan (transactionFold tx) $ toNodeStates k

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



transactionPlanner :: forall m n. (Monad m, Show n, Address n, Ord n) => [n] -> FL.Fold m (NodeStates n) (Tx n)
transactionPlanner ns = FL.Fold step start end
  where
    step ::  Tx n -> NodeStates n -> m (Tx n)
    step (Tx participants) (NodeStates nodes) = undefined
      where
        consumption = M.toAscList $ fmap _demand nodes
        storage = M.toAscList $ fmap (\n -> toWattSeconds $ (totalCapacity . _battery $ n) * (soc . _battery $ n)) nodes
        d = zipWith (\(i, c) (_, s) -> (i, c - s)) consumption storage
        (sources, sinks) = L.partition (\x -> snd x > 0) d
        better f ss = uncurry f $ unzip $ (\(x, y) -> (x, fromWattSeconds y)) <$> ss
        schedule = solveTP
          (better mkSources sources)
          (better mkSinks sinks)
          [[1 |_ <- [1..length sources]] | _ <- [1..length sinks]]
        toSourceStake t (i, e) = Stake (Source, (e2p t e), t)
        toSinkStake t (i, e) = Stake (Sink, (- e2p t e), t)
        e2p :: Time.DiffTime -> WattSeconds -> Watts
        e2p t ws = toWatts $ (fromWattSeconds ws) / (realToFrac t)
    start :: m (Tx n)
    start = pure mempty
    end :: Tx n -> m (Tx n)
    end = pure

{--
linearSolve :: (MonadIO m, Ord n) => [(n, WattSeconds)] -> [(n, WattSeconds)] -> m ([(n, Stake)])
linearSolve sources sinks = liftIO $ do
  let transportVars = [[sReal $ tName i j | i <- [1..length sources]] | j <- [1..length sinks]]
  let costs = [[1 | _ <- sources] | _ <- sinks]
  demandConstraints <- mapM constrain [((foldl (+) 0 xs) .<= (fromWS . snd $ sinks ! j)) | (j, xs) <- zip [0..length sinks - 1] transportVars]
  return []
  where
    tName i j = ("x_" <> (show i) <> "_" <> (show j))
    fromWS = pure . realToFrac . fromWattSeconds
--}
