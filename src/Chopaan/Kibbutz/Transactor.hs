{-# LANGUAGE NamedFieldPuns, OverloadedStrings, TupleSections #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveFoldable, DeriveTraversable, DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE FlexibleContexts, RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, CPP, BangPatterns #-}
module Chopaan.Kibbutz.Transactor where



import Prelude hiding (zip, zipWith)
import qualified Control.Category as C
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.DeepSeq (NFData)

import Chopaan.Utils.Streamly
import Chopaan.Node.Node (SensorR)
import Chopaan.Node.Storage.Battery (Battery(..))
import Chopaan.Node.Metrics (toWattSeconds, toWatts
                            , fromWattSeconds, fromWatts
                            , Watts, WattSeconds
                            , SensorMetrics(..)
                            , Node(..)
                            , BatteryR
                            , PowerNR
                            , pToE
                            )

import GHC.Generics (Generic)

import qualified Chopaan.Graph.Algebraic as AG
import qualified Algebra.Graph as G

import qualified Codec.Winery as W
import qualified Data.Time as Time
import qualified Data.Text as Text
import Data.Word
import Data.Maybe
import Data.Bifunctor


import Lens.Micro

import Data.ProtoLens
import Data.Convertible
import Data.Convertible.Instances ()
import Data.Aeson as A
import qualified Data.ByteString.Lazy as BL



import qualified Streamly.Prelude as S
import Streamly.Prelude (IsStream, MonadAsync, adapt)
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Pipe as P
import qualified Streamly.Internal.Data.Pipe.Type as P

import qualified Data.Map.Strict as M
import Data.Key hiding (Key)
import qualified Data.List as L

-- import Shpadoinkle.Widgets.Types (Humanize(..))

#ifndef ghcjs_HOST_OS
import Chopaan.Kibbutz.LinOpt
import Chopaan.Comm.Comm (Address(..), PubQueue, writeToPubQ)
import Data.SBV
import ConCat.Misc (R)

import Data.Greskell (lookupAs, Key, pMapToFail, FromGraphSON(..), parseGraphSON, PMapLookupException(..))
import Data.Greskell.Extra (writeKeyValues, (<=:>))
import Data.Greskell.GraphSON.GValue (unwrapOne)
import NetSpider.Found (LinkState(..))
import NetSpider.Graph (LinkAttributes(..), NodeAttributes(..), EFinds, VFoundNode)
import Chopaan.Graph.Greskell
import qualified Chopaan.Graph.Algebraic as AG
import Algebra.Graph.Label (Distance(..))
#endif



newtype Tx n a = Tx { unTx :: M.Map n a }
  deriving stock (Eq, Ord, Show, Generic, Traversable)
  deriving newtype (ToJSON, FromJSON, NFData, Functor, Foldable)
  deriving W.Serialise via (W.WineryRecord (Tx n a))

instance (Ord n) => Semigroup (Tx n a) where
  (Tx m) <> (Tx m') = Tx (m <> m')

instance (Ord n) => Monoid (Tx n a) where
  mempty = Tx mempty


type TxPlan n = Tx n Stake -- Graph Stake n

type TxState n = Tx n (Role, TxStatus) --Graph (TxStatus) (n, Role)

type NodeStates n = Tx n SensorR --Graph PowerNR (n, SensorR)


dispatchNodeTx :: forall m n. (MonadIO m, MonadCatch m, Address n)
  => PubQueue
  -> G.Graph (n, Stake)
  -> m ()
dispatchNodeTx q (Tx tx) = do
  let txDispatches =  (\(nid, st) -> (stateTopic nid, stakeToETR st)) <$> (G.vertexList tx)
  sequence_ $ (\(t, s) -> liftIO $ writeToPubQ q t s) <$> txDispatches

foldTxState :: TxState n -> (TxStatus)
foldTxState (Tx gt) = let
      gridTx = foldl (<>) mempty $ snd <$> gt
      loss = energyDispatched gridTx - energyReceived gridTx
      lossPerWS = loss / (energyDispatched gridTx)
      in gridTx{totalLoss = loss, lossPerWattSecond = lossPerWS}
{-# INLINE foldTxState #-}



transactionFold :: forall m n. (MonadIO m, MonadCatch m,  Ord n)
                => TxPlan n -> FL.Fold m (NodeStates n, Maybe (TxPlan n)) (TxState n)
transactionFold !participants = FL.foldl' incTxState (stakeStatus <$> participants)
{-# INLINE transactionFold #-}
    -- shouldQuit (Tx t) = if (all ((\x -> timeRemaining x <= 0) . snd . snd) (M.toList t))
    --                then ( . Tx $ t)
    --                else ( . Tx $ t)


stakeStatus :: Stake -> (Role, TxStatus)
stakeStatus !(Stake (!px, !w, !t)) = (px, mempty{ timeRemaining = t
                                       , energyRemaining = (pToE @Double) (realToFrac t) w
                                       , startLag = 0
                                       })
{-# INLINE stakeStatus #-}

planToState :: TxPlan n -> TxState n
planToState = fmap stakeStatus
{-# INLINE planToState #-}

zipWith3 :: (Ord n) => (a -> b -> c -> d) -> M.Map n a -> M.Map n b -> M.Map n c -> M.Map n d 
zipWith3 f a b c = M.intersectionWith ($) (M.intersectionWith f a b) c



    
transactionPlanner :: forall m n. (MonadIO m, MonadCatch m, Show n, Ord n) => Time.NominalDiffTime -> FL.Fold m (NodeStates n) (Maybe (TxPlan n))
transactionPlanner !timeHorizon = FL.foldMapM (txn timeHorizon)
{-# INLINE transactionPlanner#-}



txn :: forall m n. (MonadIO m, MonadCatch m, Ord n, Show n) => AG.Graph (Distance R) n -> Time.NominalDiffTime -> G.Graph (n, WattSeconds) -> m (G.Graph (n, Stake))
txn topology !h !(Tx ns) = do
  let nodes = M.keys ns
  let indexer = M.fromList $ zip [1..] nodes
      getAtI i = indexer M.! i
      reindexTx (Tx n) = Tx $ M.fromList $
                         fmap (\(i, a) -> (getAtI i, a)) $ M.toList n
  sched <- schedule
  return $ fmap reindexTx sched
      where
        consumption = M.toAscList $ fmap _demand ns
        storage = M.toAscList $
                  fmap (\n ->
                          (totalCapacity . _battery $ n) * (soc . _battery $ n))
                  ns
        d = fmap (\(i, (c, s))
                     -> (i, c - s)) $ zip [1..] $ zip (snd <$> consumption) (snd <$> storage)
        (sources, sinks) = L.partition (\x -> snd x > 0) d
        better f ss = uncurry f $ unzip $ (second fromWattSeconds) <$> ss
        schedule :: m (TxPlan' Int)
        schedule =  (solveTP h)
                    (better mkSources sources)
                    (better mkSinks sinks)
          [[1 | i <- [1..length sources]] | j <- [1..length sinks]]


expToMaybe :: (MonadIO m) => Either SomeException a -> m (Maybe a)
expToMaybe (Left e) = (liftIO . print $ e) >> return Nothing
expToMaybe (Right a) = return $ Just a
{-# INLINE expToMaybe #-}


type TxPlan' n = Maybe (TxPlan n)


solveTP :: forall m . (MonadIO m, MonadCatch m) => Time.NominalDiffTime -> AG.Graph (Distance Double) (WattSeconds) -> m (TxPlan' Int)
solveTP timeHorizon sources sinks = do
  liftIO $ do
    (LexicographicResult sol) <- optimize Lexicographic $ transportProblem sources sinks cs
    let dict = getModelDictionary sol
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
      e2p :: Time.NominalDiffTime -> WattSeconds -> Watts
      e2p t ws = toWatts $ (fromWattSeconds ws) / (realToFrac t)



incTxState :: (Ord n) => TxState n -> (NodeStates n, Maybe (TxPlan n)) -> TxState n
incTxState (Tx ts) (Tx ns, plan) = case plan of
  Nothing -> Tx $ zipWith updateTS ts ns
  Just (Tx p) ->
    case (M.size p == 0) of
      True -> Tx $ zipWith updateTS ts ns
      False -> Tx $ zipWith updateTS (fmap stakeStatus p) ns 
  where
    {-# INLINE updateTS #-}
    updateTS :: (Role, TxStatus) -> SensorR -> (Role, TxStatus)
    updateTS (px, prevTx) SensorMetrics{..} = (px, nextTx)
      where
        nextTS = case px of
                 Source -> (mempty @TxStatus)
                           { energyDispatched = txEnergy + energyDispatched prevTx
                           , timeRemaining = timeRemaining prevTx - lastTimeDiff
                           , energyRemaining = energyRemaining prevTx - txEnergy
                           , startLag = if hasStarted px
                                        then startLag prevTx
                                        else (startLag prevTx + lastTimeDiff)
                           , endLag = if not shouldHaveEnded
                                      then 0
                                      else (if hasEnded px
                                             then endLag prevTx
                                             else endLag prevTx + lastTimeDiff)
                           }
                 Sink -> (mempty @TxStatus)
                   { energyReceived = txEnergy + energyReceived prevTx
                   , timeRemaining = timeRemaining prevTx - lastTimeDiff
                   , energyRemaining = energyRemaining prevTx - txEnergy
                   , startLag = if hasStarted px
                                then startLag prevTx
                                else (startLag prevTx + lastTimeDiff)
                   , endLag = if not shouldHaveEnded
                              then 0
                              else (if hasEnded px
                                    then endLag prevTx
                                    else endLag prevTx + lastTimeDiff)
                   }
        txEnergy :: WattSeconds
        txEnergy = (pToE @Double) (realToFrac lastTimeDiff) (tx _powerT)
        {-# INLINE hasStarted #-}
        hasStarted Source = (abs $ tx _powerT) >= eta
        hasStarted Sink = (abs $ tx _powerT) >= eta
        {-# INLINE hasEnded #-}
        hasEnded Source =  shouldHaveEnded && (abs $ tx _powerT) <= eta
        hasEnded Sink = shouldHaveEnded && (abs $ tx _powerT) <= eta
        shouldHaveEnded = (timeRemaining prevTx) <= 0
        {-# INLINE eta #-}
        eta = 0.5
