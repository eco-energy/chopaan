{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE FlexibleContexts, RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Chopaan.Kibbutz.Transactor ( runTransactor
                                  , Stake(..)
                                  , Tx(..)
                                  , TxPlan
                                  , TxState
                                  , Role(..)
                                  , TransactionStatus(..)
                                  , foldTxState
                                  , mkStake
                                  , dispatchTx
                                  , asKbtz
                                  , planTx
                                  , monitorTx
                                  , curryTx
                                  , stakeLinkDir
                                  , txStatusLinkDir
                                  , dispatchNodeTx
                                  ) where

import Prelude hiding (zip, zipWith)

import Control.Monad.IO.Class
import Control.DeepSeq (NFData)

import Chopaan.Kibbutz.Kibbutz (Kbtz(..), kbtzState, scanKbtz, stream)
import Chopaan.Comm.Comm (Address(..), Dispatch(..), PubQueue, writeToPubQ)
import Chopaan.Node.Node (SensorS)
import Chopaan.Node.Metrics (Power
                            , toWattSeconds, toWatts
                            , fromWattSeconds, fromWatts
                            , Watts, WattSeconds
                            , SensorMetrics(..)
                            , Node(..)
                            , pToE
                            , Battery(..)
                            )


import qualified Data.Time as Time
import qualified Data.Text as Text
import Data.Word
import Data.Maybe (isJust, fromJust, fromMaybe)

import qualified Proto.NodeMessageSchema.NodeMessages as NM
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as NM

import Lens.Micro

import Data.ProtoLens
import Data.Convertible
import Data.Convertible.Instances ()
import Data.ULID
import Data.Aeson (ToJSON, FromJSON, parseJSON)

import GHC.Generics (Generic)

import qualified Streamly.Prelude as S
import Streamly
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL

import qualified Data.Map.Strict as M
import Data.Key hiding (Key)
import qualified Data.List as L

import Chopaan.Kibbutz.LinOpt
import Data.SBV
import ConCat.Misc (R)

import Data.Greskell (lookupAs, Key, pMapToFail, FromGraphSON(..), parseGraphSON)
import Data.Greskell.Extra (writeKeyValues, (<=:>))
import Data.Greskell.GraphSON.GValue (unwrapOne, unwrapAll)
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Graph (LinkAttributes(..), EFinds)


newtype Tx n a = Tx (M.Map n a)
  deriving stock (Eq, Ord, Show, Generic, Traversable)
  deriving newtype (ToJSON, FromJSON, NFData, Functor, Foldable)

instance (Ord n) => Semigroup (Tx n a) where
  (Tx m) <> (Tx m') = Tx (m <> m')

instance (Ord n) => Monoid (Tx n a) where
  mempty = Tx mempty

  

type TxPlan n = Tx n Stake

type TxState n = Tx n (Role, TransactionStatus)

type NodeStates n = Tx n SensorS



asKbtz :: forall t m n a. (IsStream t, MonadAsync m, Ord n)
  => t m (TxPlan n, t m TransactionStatus)
  -> m (Kbtz t m (TxPlan n) TransactionStatus)
asKbtz txs = do
  tx <- S.toList . adapt $ txs
  return . Kbtz . M.fromList $ tx

curryTx :: forall n a. (Address n) => a -> Tx n a -> n -> a
curryTx defA (Tx p) n = fromMaybe defA $ M.lookup n p

data TransactionStatus = TransactionStatus
  { energyDispatched :: WattSeconds
  , energyReceived :: WattSeconds
  , timeRemaining :: Time.DiffTime
  , energyRemaining :: WattSeconds
  , lossPerWattSecond :: WattSeconds
  , totalLoss :: WattSeconds
  , startLag :: Time.DiffTime
  , endLag :: Time.DiffTime
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, NFData)

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

nodeCost :: (Functor f, Functor g, Foldable f, Foldable g) => t m (TxPlan n) -> t m TransactionStatus -> FL.Fold m TransactionStatus WattSeconds -> f (g WattSeconds)
nodeCost k f = undefined

toNodeStates :: (MonadAsync m, Address n, Ord n, IsStream t, Monad (t m)) => Kbtz t m n SensorS -> t m (NodeStates n)
toNodeStates k = Tx <$> (kbtzState k)

planTx :: (MonadAsync m, Address n, Ord n, Show n, IsStream t, Monad (t m)) => Time.DiffTime -> Kbtz t m n SensorS -> t m (TxPlan n)
planTx horizon k = S.trace (\p -> liftIO . print $ "Plan For Interval:\n" <> show p) $
                   S.postscan (transactionPlanner horizon)
                    $ S.map (fromJust)
                    $ S.filter (isJust)
                    $ S.intervalsOf (realToFrac horizon) FL.last
                    $ toNodeStates k 
                   

monitorTx :: (MonadAsync m, Address n, Ord n, IsStream t, Monad (t m)) => TxPlan n -> Kbtz t m n SensorS -> t m (TxState n)
monitorTx tx k = S.postscan (transactionFold tx)
                 $ S.trace (\tx -> liftIO . print $ "Entering Tx Monitor" <> "\n" <> show tx)  
                 $ toNodeStates k



runTransactor :: (MonadAsync m, Address n, Ord n, Show n, IsStream t, Monad (t m))
  => PubQueue
  -> Time.DiffTime
  -> Kbtz t m n SensorS
  -> t m (TxPlan n, t m (TxState n))
runTransactor q horizon k = (,) <$> txs <*> statuses
  where
    txs = S.trace (dispatchTx q) $ planTx horizon k
    statuses = S.map (flip monitorTx $ k) txs
    withLog f q = f q >> \tx -> putStrLn ("Tx:\n" <> show tx) 

dispatchTx :: forall m n. (MonadIO m, Address n)
  => PubQueue
  -> TxPlan n
  -> m ()
dispatchTx q tx = do
  uid <- liftIO $ (Text.pack . show) <$> getULID
  t0 <- liftIO $ Time.getCurrentTime
  let txDispatch = mkTxDispatch uid t0 tx
  liftIO $ (writeToPubQ q) (rootTopic @n (undefined)) $ txDispatch

dispatchNodeTx :: forall m n. (MonadIO m, Address n)
  => PubQueue
  -> TxPlan n
  -> m ()
dispatchNodeTx q (Tx tx) = do
  uid <- liftIO $ (Text.pack . show) <$> getULID
  t0 <- liftIO $ Time.getCurrentTime
  let txDispatches =  (\(nid, st) -> (stateTopic nid, fromStake st)) <$> (M.toList tx)
  sequence_ $ (\(t, s) -> liftIO $ writeToPubQ q t s) <$> txDispatches


foldTxState :: (Monad m) => TxState n -> m TransactionStatus
foldTxState (Tx gt) = let
      gridTx = foldl (<>) mempty $ snd <$> gt
      loss = energyDispatched gridTx - energyReceived gridTx
      lossPerWS = loss / (energyDispatched gridTx)
      in return $ gridTx{totalLoss = loss, lossPerWattSecond = lossPerWS}

-- The state will just be carried across as a TransactionStatus
transactionFold :: forall m n. (Monad m, Address n, Ord n) => TxPlan n
  -> FL.Fold m (NodeStates n) (TxState n)
transactionFold (Tx participants) = FL.Fold step start end
  where
    step :: TxState n -> NodeStates n -> m (TxState n)
    -- Zip instance for Map is defined as intersection with, so we don't need
    -- to filter the NodeStates.
    step (Tx ts) (Tx ns) = return . Tx $ zipWith updateTS ts ns 
    start :: m (TxState n)
    start = return . Tx $
      (\(Stake (px, w, t))-> (px, mempty{ timeRemaining = t
                          , energyRemaining = (pToE @Double) (realToFrac t) w
                          , startLag = 0
                          }))
      <$> participants
    end :: TxState n -> m (TxState n)
    end = pure
    updateTS :: (Role, TransactionStatus) -> SensorS -> (Role, TransactionStatus)
    updateTS (px, txn) SensorMetrics{..} = let
      nextTS = case px of
                 Source -> (mempty @TransactionStatus)
                           { energyDispatched = txEnergy + energyDispatched txn
                           , timeRemaining = timeRemaining txn - lastTimeDiff
                           , energyRemaining = energyRemaining txn - txEnergy
                           , startLag = if hasStarted px then startLag txn else (startLag txn + lastTimeDiff)
                           , endLag = if hasEnded px && shouldHaveEnded then 0 else lastTimeDiff
                           }
                 Sink -> (mempty @TransactionStatus)
                   { energyReceived = txEnergy + energyReceived txn
                   , timeRemaining = timeRemaining txn - lastTimeDiff
                   , energyRemaining = energyRemaining txn - txEnergy
                   , startLag = if hasStarted px then startLag txn else (startLag txn + lastTimeDiff)
                   , endLag = if not shouldHaveEnded then 0 else (if hasEnded px then endLag txn else endLag txn + lastTimeDiff)
                   }
      in (px, nextTS)
      where
        txEnergy :: WattSeconds
        txEnergy = (pToE @Double) (realToFrac lastTimeDiff) (tx _powerT)
        hasStarted Source = (abs $ tx _powerT) >= eta
        hasStarted Sink = (abs $ tx _powerT) >= eta
        hasEnded Source =  shouldHaveEnded && (abs $ tx _powerT) <= eta
        hasEnded Sink = shouldHaveEnded && (abs $ tx _powerT) <= eta
        shouldHaveEnded = (energyRemaining txn - txEnergy) <= 0
        eta = 0.5

transactionPlanner :: forall m n. (MonadIO m, Show n, Address n, Ord n) => Time.DiffTime -> FL.Fold m (NodeStates n) (TxPlan n)
transactionPlanner timeHorizon = FL.Fold step start end
  where
    step ::  TxPlan n -> NodeStates n -> m (TxPlan n)
    step (Tx _) (Tx nodes) = do
      s <- schedule
      case s of
        (ValidPlan sc c) -> return $ sc
        Wait -> return $ mempty
      where
        consumption = M.toAscList $ fmap _demand nodes
        storage = M.toAscList $ fmap (\n -> toWattSeconds $ (totalCapacity . _battery $ n) * (soc . _battery $ n)) nodes
        d = zipWith (\(i, c) (_, s) -> (i, c - s)) consumption storage
        (sources, sinks) = L.partition (\x -> snd x > 0) d
        better f ss = uncurry f $ unzip $ (\(x, y) -> (x, fromWattSeconds y)) <$> ss
        schedule :: m (TxPlan' n)
        schedule = solveTP timeHorizon
          (better mkSources sources)
          (better mkSinks sinks)
          [[1 |_ <- [1..length sources]] | _ <- [1..length sinks]]        
    start :: m (TxPlan n)
    start = pure mempty
    end :: TxPlan n -> m (TxPlan n)
    end = pure

data TxPlan' n = ValidPlan (TxPlan n) R | Wait

solveTP :: forall m n. (MonadIO m, Ord n, Show n) => Time.DiffTime -> Sources n -> Sinks n -> [[Double]] -> m (TxPlan' n)
solveTP timeHorizon sources sinks cs = do
  liftIO $ do
    (LexicographicResult sol) <- optimize Lexicographic $ transportProblem sources sinks cs
    let dict = getModelDictionary sol
    print $ "Plan:\n" <> (show dict)
    if not . modelExists $ sol then return Wait else do
      let
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
      --print ("Plan Made: " <> show dict)
      return . (flip ValidPlan 0) . Tx . M.fromList $ (filter isZeroStake asSources) <> (filter isZeroStake asSinks)  
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

isValidPlan :: [a] -> Bool
isValidPlan plan = length plan > 0


{---
    Concretely
----}

data Role = Source | Sink
  deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

instance FromGraphSON Role where
  parseGraphSON = parseJSON . unwrapOne

newtype Stake = Stake {
  unStake :: (Role, Watts, Time.DiffTime)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (NFData, ToJSON, FromJSON)


roleKey :: Key EFinds Role
roleKey = "role"

powerKey :: Key EFinds Watts
powerKey = "power"

durationKey :: Key EFinds Time.DiffTime
durationKey = "duration"


instance LinkAttributes Stake where
  writeLinkAttributes (Stake (r, w, t)) = fmap writeKeyValues $
    sequence [ (roleKey <=:> r)
             , (powerKey <=:> w)
             , (durationKey <=:> t)
             ]
  parseLinkAttributes props = pMapToFail (Stake <$> tupleUp)
    where
      tupleUp = tup3
                <$> lookupAs roleKey props
                <*> lookupAs powerKey props
                <*> lookupAs durationKey props
      tup3 a b c = (a, b, c) 

roleLinkDir :: Role -> LinkState
roleLinkDir r = case r of
  Source -> LinkToTarget
  Sink -> LinkToSubject

stakeLinkDir :: Stake -> LinkState
stakeLinkDir (Stake (r, _, _)) = roleLinkDir r

txStatusLinkDir :: TransactionStatus -> LinkState
txStatusLinkDir TransactionStatus{energyDispatched, energyReceived} = if energyDispatched > 0 && energyDispatched == 0
  then LinkToTarget
  else if energyReceived > 0 && energyDispatched == 0
       then LinkToSubject
       else LinkBidirectional

instance Semigroup Stake where
  (Stake (Source, w, t)) <> (Stake (Source, w', t')) = Stake (Source, w + w', t + t')
  (Stake (Source, w, t)) <> (Stake (Sink, w', t')) = Stake (role, w'', t + t')
    where
      w'' = abs $ w - w'
      role = if w - w' > 0 then Source else Sink
  (Stake (Sink, w, t)) <> (Stake (Source, w', t')) = Stake (role, w'', t + t')
    where
      w'' = abs $ w - w'
      role = if w - w' > 0 then Source else Sink
  (Stake (Sink, w, t)) <> (Stake (Sink, w', t')) = Stake (Sink, abs $ w + w', t + t')

instance Monoid Stake where
  mempty = Stake (Sink, 0, 0)

mkStake :: Role -> Double -> Int -> Stake
mkStake r p t = Stake (r, toWatts p, fromIntegral t)

mkTxDispatch :: (Address n) => Text.Text -> Time.UTCTime -> TxPlan n -> NM.Transaction
mkTxDispatch uid stime (Tx txns) = defMessage
                         & NM.start .~ (utcToWord64 stime)
                         & NM.etrs .~ (M.mapKeys (toRemoteId) $ fromStake <$> txns) 
  where
    utcToWord64 :: Time.UTCTime -> Word64
    utcToWord64 = (convert @Int @Word64) . (convert @Time.UTCTime @Int)

fromStake :: Stake -> NM.EnergyTransactionRequest
fromStake (Stake (role, watts, duration)) = defMessage
                                            & NM.powerInWatts .~ (fromWatts watts)
                                            & NM.durationInSeconds .~ (timeToWord duration)
                                            & NM.direction .~ (toPDir role) 
  where
    toPDir Source = NM.Outgoing
    toPDir Sink = NM.Incoming
    timeToWord :: Time.DiffTime -> Word64
    timeToWord = (convert @Int @Word64) . (round @Time.DiffTime @Int)



keyED :: Key EFinds WattSeconds
keyED = "energyDispatched"

keyERec :: Key EFinds WattSeconds
keyERec = "energyReceived"

keyTR :: Key EFinds Time.DiffTime
keyTR = "timeRemaining"

keyERem :: Key EFinds WattSeconds
keyERem = "energyRemaining"

keyLPW :: Key EFinds WattSeconds
keyLPW = "lossPerWattSecond"

keyTL :: Key EFinds WattSeconds
keyTL = "totalLoss"

keySL :: Key EFinds Time.DiffTime
keySL = "stateLag"

keyEL :: Key EFinds Time.DiffTime
keyEL = "endLag"


instance LinkAttributes TransactionStatus where
  writeLinkAttributes TransactionStatus{..} = fmap writeKeyValues $ sequence $
    [ keyED <=:> energyDispatched
    , keyERec <=:> energyReceived
    , keyTR <=:> timeRemaining
    , keyERem <=:> energyRemaining
    , keyLPW <=:> lossPerWattSecond
    , keyTL <=:> totalLoss
    , keySL <=:> startLag
    , keyEL <=:> endLag
    ]
  parseLinkAttributes props =
    pMapToFail (TransactionStatus
                <$> lookupAs keyED props
                <*> lookupAs keyERec props
                <*> lookupAs keyTR props
                <*> lookupAs keyERem props
                <*> lookupAs keyLPW props
                <*> lookupAs keyTL props
                <*> lookupAs keySL props
                <*> lookupAs keyEL props)
