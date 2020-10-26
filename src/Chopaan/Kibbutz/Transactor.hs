{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE FlexibleContexts, RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Chopaan.Kibbutz.Transactor (runTransactor, Stake(..), Tx(..), Role(..), TransactionStatus(..)) where

import Prelude hiding (zip, zipWith)

import Control.Monad.IO.Class

import Chopaan.Kibbutz.Kibbutz (Kbtz(..), asMapStream)
import Chopaan.Comm.Comm (Address(..), Dispatch(..), PubQueue, writeToPubQ)
import Chopaan.Node.Node (pToE, toWattSeconds, toWatts, fromWattSeconds, fromWatts, Watts, WattSeconds, NodeS, NodeMetrics(..), Power(..), Battery(..))

import qualified Data.Time as Time
import qualified Data.Text as Text
import Data.Word
import Data.Maybe (isNothing, fromJust)

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

import Chopaan.Kibbutz.LinOpt

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


fromStake :: (MonadIO m) => Stake -> m (NM.EnergyTransactionRequest)
fromStake (Stake (role, watts, duration)) = do
  uid <- liftIO $ getULID
  t0 <- liftIO $ Time.getCurrentTime
  return $ mkETR watts duration (toPDir role) (Text.pack . show $ uid) t0
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

toNodeStates :: (Monad m, Address n, Ord n, IsStream t, Monad (t m)) => Kbtz t m n NodeS -> t m (NodeStates n)
toNodeStates k = NodeStates <$> asMapStream k

planTx :: (MonadAsync m, Address n, Ord n, Show n, IsStream t, Monad (t m)) => Time.DiffTime -> Kbtz t m n NodeS -> t m (Tx n)
planTx horizon k = S.postscan (transactionPlanner horizon) $ S.map (fromJust) $ S.filter (isNothing) $ S.intervalsOf (realToFrac horizon) FL.last $ toNodeStates k -- apply intervals of to t m NodeS instead of t m (Tx n)

monitorTx :: (Monad m, Address n, Ord n, IsStream t, Monad (t m)) => Tx n -> Kbtz t m n NodeS -> t m (TransactionStatus)
monitorTx tx k = S.postscan (transactionFold tx) $ toNodeStates k


runTransactor :: (MonadAsync m, Address n, Ord n, Show n, IsStream t, Monad (t m))
  => (PubQueue n NM.MeshFrame)
  -> Time.DiffTime
  -> Kbtz t m n NodeS
  -> m (t m TransactionStatus, t m (Tx n))
runTransactor q horizon k = return (statuses, txs)
  where
    txs = S.trace (dispatchTx q) $ planTx horizon k
    statuses = S.concatMap (flip monitorTx $ k) txs 

dispatchTx :: (MonadIO m, Address n)
  => (PubQueue n NM.MeshFrame)
  -> Tx n
  -> m ()
dispatchTx q (Tx tx) = do
  c <- traverse (liftIO . fromStake) tx
  liftIO $ mapM_ (uncurry $ writeToPubQ q) $ M.toList $ frame <$> c

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
                          , energyRemaining = (pToE @Double) (realToFrac t) w
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
        e = (pToE @Double) (realToFrac lastTimeDiff) (tOutP _powerT)
        hasStarted Source = (tOutP _powerT) >= eta
        hasStarted Sink = (tInP _powerT) >= eta
        hasEnded Source =  shouldHaveEnded && (tOutP _powerT) <= eta
        hasEnded Sink = shouldHaveEnded && (tInP _powerT) <= eta
        shouldHaveEnded = (energyRemaining tx - e) <= 0
        eta = 0.5

transactionPlanner :: forall m n. (MonadIO m, Show n, Address n, Ord n) => Time.DiffTime -> FL.Fold m (NodeStates n) (Tx n)
transactionPlanner timeHorizon = FL.Fold step start end
  where
    step ::  Tx n -> NodeStates n -> m (Tx n)
    step (Tx _) (NodeStates nodes) = schedule
      where
        consumption = M.toAscList $ fmap _demand nodes
        storage = M.toAscList $ fmap (\n -> toWattSeconds $ (totalCapacity . _battery $ n) * (soc . _battery $ n)) nodes
        d = zipWith (\(i, c) (_, s) -> (i, c - s)) consumption storage
        (sources, sinks) = L.partition (\x -> snd x > 0) d
        better f ss = uncurry f $ unzip $ (\(x, y) -> (x, fromWattSeconds y)) <$> ss
        schedule :: m (Tx n)
        schedule = solveTP timeHorizon
          (better mkSources sources)
          (better mkSinks sinks)
          [[1 |_ <- [1..length sources]] | _ <- [1..length sinks]]        
    start :: m (Tx n)
    start = pure mempty
    end :: Tx n -> m (Tx n)
    end = pure

solveTP :: forall m n. (MonadIO m, Ord n, Show n) => Time.DiffTime -> Sources n -> Sinks n -> [[Double]] -> m (Tx n)
solveTP timeHorizon sources sinks cs = liftIO $ do
  (LexicographicResult sol) <- optimize Lexicographic $ transportProblem sources sinks cs
  let dict = getModelDictionary sol
      (ns, cvs) = unzip $ M.toAscList dict
      vs' :: M.Map String Double
      vs' = M.fromAscList $ zip ns (parseToDoubles cvs)
      toTransferMat :: M.Map String Double -> [[Double]]
      toTransferMat m = (zipWith (zipWith (+))) ((fmap (fmap (* (-1)))) . L.transpose $ x') x'
        where
          x' = [[zeroIfNone $ M.lookup (tName i j) m | i <- getNames sources] | j <- getNames sinks]
      transferMat = toTransferMat vs'
      sourceTransmit = (toWattSeconds . abs) <$> (fmap sum $ L.transpose transferMat)
      sinkReceive = (toWattSeconds . abs) <$> (fmap sum $ transferMat)
      asSources = toSourceStake timeHorizon <$> (zip (getNames sources) sourceTransmit)
      asSinks = toSinkStake timeHorizon <$> (zip (getNames sinks) sinkReceive)
  return . Tx . M.fromList $ (filter isZeroStake asSources) <> (filter isZeroStake asSinks)
  where
  isZeroStake (_, (Stake (_, a, t))) = a > 0 && t > 0 
  zeroIfNone Nothing = 0
  zeroIfNone (Just a) = a
  parseToDoubles ys = case (parseCVs @Double) ys of
                Just (a, rs) -> (a:parseToDoubles rs)
                Nothing -> []
  toSourceStake t (i, e) = (i, Stake (Source, (e2p t e), t))
  toSinkStake t (i, e) = (i, Stake (Sink, (- e2p t e), t))

e2p :: Time.DiffTime -> WattSeconds -> Watts
e2p t ws = toWatts $ (fromWattSeconds ws) / (realToFrac t)
