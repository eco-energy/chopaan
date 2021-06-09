{-# LANGUAGE NamedFieldPuns, OverloadedStrings, TupleSections #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE FlexibleContexts, RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Chopaan.Kibbutz.Transactor--  ( -- runTransactor
--                                   statePipe
--                                   , Stake(..)
--                                   , Tx(..)
--                                   , TxPlan
--                                   , TxState
--                                   , Role(..)
--                                   , TransactionStatus(..)
--                                   , foldTxState
--                                   , mkStake
--                                   , dispatchTx
-- --                                  , asKbtz
--                                   , planTx
--                                   --, monitorTx
--                                   , curryTx
--                                   , stakeLinkDir
--                                   , txStatusLinkDir
--                                   , dispatchNodeTx
--                                   ) 
where

import Prelude hiding ((.), id, zip, zipWith)
import Control.Category
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.DeepSeq (NFData)

import Chopaan.Utils.Streamly
import Chopaan.Kibbutz.Kibbutz (Kbtz(..))
import Chopaan.Comm.Comm (Address(..), PubQueue, writeToPubQ)
import Chopaan.Node.Node (SensorS)
import Chopaan.Node.Metrics (toWattSeconds, toWatts
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
import Data.Maybe
import Data.Bifunctor
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
import Streamly.Prelude (IsStream, MonadAsync, adapt)
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Pipe as P
import qualified Streamly.Internal.Data.Pipe.Types as P

import qualified Data.Map.Strict as M
import Data.Key hiding (Key)
import qualified Data.List as L

import Chopaan.Kibbutz.LinOpt
import Data.SBV
import ConCat.Misc (R)

import Data.Greskell (lookupAs, Key, pMapToFail, FromGraphSON(..), parseGraphSON)
import Data.Greskell.Extra (writeKeyValues, (<=:>))
import Data.Greskell.GraphSON.GValue (unwrapOne)
import NetSpider.Found (LinkState(..))
import NetSpider.Graph (LinkAttributes(..), EFinds)


newtype Tx n a = Tx { unTx :: M.Map n a }
  deriving stock (Eq, Ord, Show, Generic, Traversable)
  deriving newtype (ToJSON, FromJSON, NFData, Functor, Foldable)

instance (Ord n) => Semigroup (Tx n a) where
  (Tx m) <> (Tx m') = Tx (m <> m')

instance (Ord n) => Monoid (Tx n a) where
  mempty = Tx mempty


type TxPlan n = Tx n Stake

type TxState n = Tx n (Role, TransactionStatus)

type NodeStates n = Tx n SensorS



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


toNodeStates :: (MonadAsync m, Address n, Ord n, IsStream t) => Kbtz t m n SensorS -> t m (NodeStates n)
toNodeStates k = Tx <$> (unKibbutz k)


planTx :: (MonadAsync m, MonadCatch m, Address n, Ord n, Show n, IsStream t) => Time.DiffTime -> t m (NodeStates n) -> t m (Maybe (TxPlan n))
planTx horizon k = S.postscan (transactionPlanner horizon) k 
                   



-- We need a pipe that goes from the Kibbutz to the 

-- monitorTx :: (MonadAsync m, Address n, Ord n, IsStream t)
--   => (TxPlan n)
--   -> Kbtz t m n SensorS
--   -> t m (TxState n)
-- monitorTx tx (Kbtz k) = do
--   (k', k'') <- duplicateS k
--   S.postscan (transactionFold tx) $ toNodeStates (Kbtz k')

-- -- Same Kibbutz is being passed to each Tx Monitor!!

-- transactor :: (IsStream t, MonadAsync m, Address n, Ord n, Show n)
--   => Time.DiffTime
--   -> Kbtz t m n SensorS
--   -> P.Pipe m (n, SensorS) (n, SensorS, TxPlan n, TxState n)
-- transactor h (Kbtz k) = do
--   (planS, monS) <- duplicateS k
--   let plans = planTx h (Kbtz planS)
--   -- S.mapM monitorTx 
--   --S.iterateMapM (flip monitorTx (Kbtz monS)) plans
--   undefined

-- runTransactor :: (MonadAsync m, Address n, Ord n, Show n, IsStream t)
--   => PubQueue
--   -> Time.DiffTime
--   -> Kbtz t m n SensorS
--   -> t m (TxPlan n, t m (TxState n))
-- runTransactor q horizon k = S.zipWith (,) txs statuses
--   where
--     txs = S.trace (dispatchTx q) $ planTx horizon k
--     statuses = S.concatMap (flip monitorTx $ k) txs

dispatchTx :: forall m n. (MonadIO m, MonadCatch m, Address n)
  => PubQueue
  -> TxPlan n
  -> m ()
dispatchTx = dispatchNodeTx -- do
  -- uid <- liftIO $ (Text.pack . show) <$> getULID
  -- t0 <- liftIO $ Time.getCurrentTime
  -- let txDispatch = mkTxDispatch uid t0 tx
  -- liftIO $ (writeToPubQ q) (rootTopic @n (undefined)) $ txDispatch

dispatchNodeTx :: forall m n. (MonadIO m, MonadCatch m, Address n)
  => PubQueue
  -> TxPlan n
  -> m ()
dispatchNodeTx q (Tx tx) = do
  let txDispatches =  (\(nid, st) -> (stateTopic nid, fromStake st)) <$> (M.toList tx)
  sequence_ $ (\(t, s) -> liftIO $ writeToPubQ q t s) <$> txDispatches


foldTxState :: TxState n -> TransactionStatus
foldTxState (Tx gt) = let
      gridTx = foldl (<>) mempty $ snd <$> gt
      loss = energyDispatched gridTx - energyReceived gridTx
      lossPerWS = loss / (energyDispatched gridTx)
      in gridTx{totalLoss = loss, lossPerWattSecond = lossPerWS}

-- The state will just be carried across as a TransactionStatus
transactionFold :: forall m n. (MonadIO m, MonadCatch m, Address n, Ord n) => TxPlan n
  -> FL.Fold m (NodeStates n) (TxState n)
transactionFold participants = FL.Fold step start end
  where
    step t n = pure . shouldQuit $ incTxState t n
    start :: m (TxState n)
    start = return $ stakeStatus <$> participants
    end :: TxState n -> m (TxState n)
    end = pure
    shouldQuit (Tx t) = if (all ((\x -> timeRemaining x <= 0) . snd . snd) (M.toList t))
                   then (FL.Done . Tx $ t)
                   else (FL.Partial . Tx $ t)


stakeStatus :: Stake -> (Role, TransactionStatus)
stakeStatus (Stake (px, w, t)) = (px, mempty{ timeRemaining = t
                                       , energyRemaining = (pToE @Double) (realToFrac t) w
                                       , startLag = 0
                                       })

planToState :: TxPlan n -> TxState n
planToState = fmap stakeStatus

incTxState :: (Ord n) => TxState n -> NodeStates n -> TxState n
incTxState (Tx ts) (Tx ns) = Tx $ zipWith updateTS ts ns
  where
    updateTS :: (Role, TransactionStatus) -> SensorS -> (Role, TransactionStatus)
    updateTS (px, prevTx) SensorMetrics{..} = let
      nextTS = case px of
                 Source -> (mempty @TransactionStatus)
                           { energyDispatched = txEnergy + energyDispatched prevTx
                           , timeRemaining = timeRemaining prevTx - lastTimeDiff
                           , energyRemaining = energyRemaining prevTx - txEnergy
                           , startLag = if hasStarted px then startLag prevTx else (startLag prevTx + lastTimeDiff)
                           , endLag = if not shouldHaveEnded then 0 else (if hasEnded px then endLag prevTx else endLag prevTx + lastTimeDiff)
                           }
                 Sink -> (mempty @TransactionStatus)
                   { energyReceived = txEnergy + energyReceived prevTx
                   , timeRemaining = timeRemaining prevTx - lastTimeDiff
                   , energyRemaining = energyRemaining prevTx - txEnergy
                   , startLag = if hasStarted px then startLag prevTx else (startLag prevTx + lastTimeDiff)
                   , endLag = if not shouldHaveEnded then 0 else (if hasEnded px then endLag prevTx else endLag prevTx + lastTimeDiff)
                   }
      in (px, nextTS)
      where
        txEnergy :: WattSeconds
        txEnergy = (pToE @Double) (realToFrac lastTimeDiff) (tx _powerT)
        hasStarted Source = (abs $ tx _powerT) >= eta
        hasStarted Sink = (abs $ tx _powerT) >= eta
        hasEnded Source =  shouldHaveEnded && (abs $ tx _powerT) <= eta
        hasEnded Sink = shouldHaveEnded && (abs $ tx _powerT) <= eta
        shouldHaveEnded = (timeRemaining prevTx) <= 0
        eta = 0.5

    
-- transactor :: forall m n. P.Pipe m (NodeStates n) (TxPlan n, TxState)
-- transactor = P.Pipe consumer producer i
--   where
--     i = undefined
--     consumer :: TxState n -> (NodeStates n) -> m (P.Step (P.PipeState x TxState n)) (TxPlan, TxState)
--     consumer p n = pure $ P.Yield () 
--     producer :: y -> m (P.Step (P.PipeState TxPlan y) (TxPlan, TxState))
--     producer = undefined


planPipe :: forall m n. (MonadIO m, MonadCatch m, Address n, Ord n)
  => Time.DiffTime -> P.Pipe m (NodeStates n) (Maybe (TxPlan n))
planPipe = P.mapM . txn

planToStatus :: forall m n. (MonadIO m, MonadCatch m, Address n, Ord n)
             => P.Pipe m (Maybe (TxPlan n)) (Maybe (TxPlan n, TxState n)) 
planToStatus = P.zipWith (\x y -> (,) <$> x <*> y) id (P.map (fmap planToState))

ntos :: (MonadIO m, MonadCatch m, Address n, Ord n) => Time.DiffTime ->  P.Pipe m (NodeStates n) (Maybe (TxPlan n, TxState n))
ntos t = planToStatus . (planPipe t)

statePipe :: (MonadIO m, MonadCatch m, Address n, Ord n) => Time.DiffTime -> P.Pipe m (NodeStates n) (Maybe (NodeStates n, TxPlan n, TxState n))
statePipe t = (P.zipWith status (ntos t) (P.map Just))

statusPipe :: forall m n. (MonadIO m, MonadCatch m, Address n, Ord n)
  => P.Pipe m (TxState n) (NodeStates n -> TxState n)
statusPipe = P.map incTxState

--stateP = P.zipWith statusPipe 
--applyInPipe = 

status :: (Address n, Ord n)
  => Maybe (TxPlan n, TxState n)
  -> Maybe (NodeStates n)
  -> Maybe (NodeStates n, TxPlan n, TxState n)
status x y = (,,) <$> y <*> (fst <$> x) <*> (liftA2 incTxState (snd <$> x) y)



transactionPlanner :: forall m n. (MonadIO m, MonadCatch m, Show n, Address n, Ord n) => Time.DiffTime -> FL.Fold m (NodeStates n) (Maybe (TxPlan n))
transactionPlanner timeHorizon = FL.Fold step start end
  where
    step _ n = (pure . FL.Partial) =<< txn timeHorizon n
    start = pure mempty
    end = pure


txn' h t = (pure . (fromMaybe mempty)) =<< txn h t

txn :: forall m n. (MonadIO m, MonadCatch m, Ord n, Show n) => Time.DiffTime -> NodeStates n -> m (TxPlan' n)
txn h (Tx ns) = do
  -- liftIO . print $ (better mkSources sources)
  -- liftIO . print $ (better mkSinks sinks)
  -- liftIO . print $ d
  let nodes = M.keys ns
  let indexer = M.fromList $ zip [1..] nodes
      getAtI i = indexer M.! i
  fmap (fmap ((\(Tx n) -> Tx $ M.fromList $
                fmap (\(i, a) -> (getAtI i, a)) $ M.toList n))) schedule
      where
        consumption = M.toAscList $ fmap _demand ns
        storage = M.toAscList $
                  fmap (\n ->
                          toWattSeconds $
                          (totalCapacity . _battery $ n) * (soc . _battery $ n))
                  ns
        d = fmap (\(i, (c, s))
                     -> (i, c - s)) $ zip [1..] $ zip (snd <$> consumption) (snd <$> storage)
        (sources, sinks) = L.partition (\x -> snd x > 0) d
        better f ss = uncurry f $ unzip $ (\(x, y) -> (x, fromWattSeconds y)) <$> ss
        schedule :: m (TxPlan' Int)
        schedule =  (fmap join) . tryForMaybe $ (solveTP h)
                    (better mkSources sources)
                    (better mkSinks sinks)
          [[1 -- (fromIntegral $ mod j 2) * 1000
           |i <- [1..length sources]] | j <- [1..length sinks]]

tryForMaybe :: (MonadIO m, MonadCatch m) => m a -> m (Maybe a) 
tryForMaybe m = expToMaybe =<< (try m)

expToMaybe :: (MonadIO m) => Either SomeException a -> m (Maybe a)
expToMaybe (Left e) = (liftIO . print $ e) >> return Nothing
expToMaybe (Right a) = return $ Just a

type TxPlan' n = Maybe (TxPlan n)

solveTP :: forall m . (MonadIO m, MonadCatch m) => Time.DiffTime -> Sources Int -> Sinks Int -> [[Double]] -> m (TxPlan' Int)
solveTP timeHorizon sources sinks cs = do
  liftIO $ do
    (LexicographicResult sol) <- optimize Lexicographic $ transportProblem sources sinks cs
    -- let
    --   pSol (Unsatisfiable _ x) = print "Unsatisfiable"-- >> print x
    --   pSol (Satisfiable _ m) = print "Satisfiable!"-- >> print m
    --   pSol (SatExtField _ m) = print "Satisfies Extension Field Only!" >> print m
    --   pSol (Unknown _ s) = print "Unknown!" >> print s
    --   pSol (ProofError _ s _) = print "Proof Error!" >> print s
      
      
    -- liftIO . pSol $ sol
    let dict = getModelDictionary sol
    --liftIO . print $ "Plan:\n" <> (show dict)
    if not . modelExists $ sol then return Nothing else do
      let (ns, cvs) = unzip $ M.toAscList dict
      --liftIO . print $ dict
      case ((M.lookup "goal" dict)) of
        Nothing -> return Nothing
        Just x -> do
          let
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
            planDict = M.fromList $ (asSources) <> (asSinks)
          --print ("Plan Dict: " <> show planDict)
          return . Just . Tx $ planDict
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
