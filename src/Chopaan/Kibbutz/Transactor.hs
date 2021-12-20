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
import Data.Monoid
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


import Chopaan.Kibbutz.LinOpt
import Chopaan.Kibbutz.Transactor.Stake
import Chopaan.Kibbutz.Transactor.Status
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
dispatchNodeTx q tx = do
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


newtype EnergyDemand = EnergyDemand (WattSeconds)
  deriving (Eq, Ord, Show)
  deriving newtype (Num, Fractional, Real, RealFrac)

nodeDemand :: SensorR -> EnergyDemand
nodeDemand SensorMetrics{_demand, _battery} = EnergyDemand ((stored _battery) - _demand)
  where
    stored Battery{totalCapacity, soc} = totalCapacity * soc

txn :: forall m n. (MonadIO m, MonadCatch m, MonadFail m, Ord n, HasVarName n)
  => Time.NominalDiffTime
  -> AG.Graph (Distance R) n
  -> G.Graph (n, SensorR)
  -> m (AG.Graph Stake n)
txn h topology state = (first toStake)
                       <$> (solveTP h topology (fmap (second (realToFrac . nodeDemand)) state))

toStake :: R -> Stake
toStake = undefined

expToMaybe :: (MonadIO m) => Either SomeException a -> m (Maybe a)
expToMaybe (Left e) = (liftIO . print $ e) >> return Nothing
expToMaybe (Right a) = return $ Just a
{-# INLINE expToMaybe #-}

--plan horizon = S.postscan (secondF (dupF (transactionPlanner horizon)))
--status ns = S.postscan (secondF (txFold (Tx . M.fromList $ [(n, mempty @Stake) | n <- ns])))

type TxPlan' n = Maybe (TxPlan n)



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
    updateTS (px, prevTx) SensorMetrics{..} = (px, nextTS)
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
