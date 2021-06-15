{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes, ConstraintKinds, KindSignatures, QuantifiedConstraints, MultiParamTypeClasses, GADTs, FlexibleInstances#-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns, NoMonomorphismRestriction  #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveAnyClass, DeriveFunctor, StandaloneDeriving #-}

{-# LANGUAGE ExplicitForAll, FlexibleContexts, TupleSections #-}
module Chopaan.Graph.Spider where

import qualified Streamly.Prelude as S
import Streamly.Prelude
import qualified Streamly.Internal.Data.Fold as FL

import GHC.Generics
import Control.DeepSeq
import Control.Monad.IO.Class
import Control.Monad.Catch

import Chopaan.Node.NodeId
import Chopaan.Node.Metrics hiding (Timestamp)
import Chopaan.Node.Folds
import Chopaan.Node.Mesh (MeshNode, RxSignal, rsToFN, initMeshNode)
import qualified Proto.NodeMessageSchema.NodeMessages as N

import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.Kibbutz
import Chopaan.Kibbutz.Transactor ( TransactionStatus
                                  , Stake
                                  , stakeLinkDir
                                  , txStatusLinkDir
                                  )
import Chopaan.Graph.Greskell

import NetSpider.Spider (Spider, addFoundNode, getSnapshot, getSnapshotSimple, connectWith, close, withSpider)
import NetSpider.Spider.Config (Config(..), defConfig)
import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..))
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Timestamp (fromUTCTime, now, Timestamp)
import NetSpider.Snapshot (SnapshotGraph)
import NetSpider.Query (defQuery, Query(..), Extended(..), (<=..<=), policyAppend)


import Data.Aeson (ToJSON, FromJSON)
import Data.Greskell
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, getCurrentTime)
import Data.Pool

import Control.Monad.Trans.Reader



type SpiderConn n v e = (SpiderNodeId n, NodeAttributes v, LinkAttributes e, Show n, Show v, Show e)

type SpiderM m n c d = ReaderT (Spider n c d) m


type SpiderNodeId n = (ToJSON n)

newtype KbtzRoot n = KbtzRoot { getRoot :: n }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

mkKbtzRoot :: KbtzName -> KbtzRoot NodeMAC
mkKbtzRoot (KbtzId k) = (KbtzRoot (NodeId ("grid_" <> k) :: NodeMAC))

getGridRoot :: KbtzName -> NodeMAC
getGridRoot = getRoot . mkKbtzRoot
  

class (NodeAttributes v) => HasTime v where
  getVTime :: v -> Maybe UTCTime

instance (GreskellC e, GreskellC p) => HasTime (SensorMetrics e p) where
  getVTime = _time

class (LinkAttributes e) => HasDir e where
  getEDir :: e -> LinkState

instance HasDir Stake where
  getEDir = stakeLinkDir

instance HasDir TransactionStatus where
  getEDir = txStatusLinkDir
  
type GrSConn t m n v e = (IsStream t, MonadAsync m, SpiderConn n v e, HasTime v, HasDir e)

type GrS t m n v e = GrSConn t m n v e => t m (n, v, [e])


ingestHyperGraph :: forall t m n v e.
  (KbtzConn t m n, MonadCatch m, SpiderConn n v e, HasTime v, HasDir e)
  => Config n v e
  -> KbtzRoot n -- $ NodeId Representing the Grid Root, serves as common edge for the hypergraph representation (Maybe?)
  -> GrS t m n v e -- $ a stream of vertices and a stream of edges for each vertex
  -> t m Bool
ingestHyperGraph conf (KbtzRoot gn) =
  writeSpiderStream conf (\s -> (addNodeWithEdges s))
  where
    addNodeWithEdges :: Spider n v e -> (n, v, [e]) -> m Bool
    addNodeWithEdges spider (n, v, es) = do
      liftIO . (print @String) $ "adding a node"
      t' <- liftIO now
      let
        t = fromMaybe t' $ fromUTCTime <$> (getVTime v)
        lx = toLink gn <$> es
      addFN spider $ toFN t n v lx 


addFN :: MonadIO m
      => MonadCatch m
      => SpiderConn n v e
      => Spider n v e -> FoundNode n v e -> m (Bool) 
addFN s f = expToBool =<< (liftIO $ (print f) >>
                           (try (addFoundNode s f)))

tryForBool :: (MonadIO m, MonadCatch m) => m a -> m Bool 
tryForBool m = expToBool =<< (try m)

expToBool :: (MonadIO m) => Either SomeException a -> m Bool
expToBool (Left e) = (liftIO . print $ e) >> return False
expToBool (Right _) = return True


toFN :: (SpiderConn n v e) => Timestamp -> n -> v -> [FoundLink n e] -> FoundNode n v e
toFN t n v lx = FoundNode
      { subjectNode = n
      , foundAt = t 
      , neighborLinks = lx
      , nodeAttributes = v
      }

toLink :: HasDir e => n -> e -> FoundLink n e
toLink n' e = FoundLink
                     { targetNode = n'
                     , linkState = getEDir e
                     , linkAttributes = e
                     }


spiderFold :: forall m a n v e. (MonadAsync m, MonadCatch m, SpiderConn n v e)
           => Config n v e
           -> ((n, a) -> m (FoundNode n v e))
           -> FL.Fold m (n, a) Bool
spiderFold conf asFN = FL.mkFoldM step start end
  where
    step :: (Pool(Spider n v e), Bool) -> (n, a) -> m (FL.Step (Pool(Spider n v e), Bool) Bool)
    step (s, _) a = (\r -> return $ FL.Partial (s, r))
      =<< (withResource s . (flip addFN))
      =<< (asFN a)
    start = (,True) <$> (liftIO $ spiderPool conf)
    end (s, r) = liftIO $ destroyAllResources s >> return r


addMeshNode :: forall m. (MonadAsync m, MonadCatch m)
  => FL.Fold m (NodeMAC, N.RuntimeStats) Bool
addMeshNode = spiderFold meshConfig (pure . rsToFN)

addStakeNode :: forall m. (MonadAsync m, MonadCatch m)
  => KbtzRoot NodeMAC -> FL.Fold m (NodeMAC, (SensorS, Stake, TransactionStatus)) Bool
addStakeNode (KbtzRoot gn) = spiderFold stakeConfig (\(n, (s, st, _)) -> x n s st)
  where
    x :: NodeMAC -> SensorS -> Stake -> m (FoundNode NodeMAC SensorS Stake)
    x n v e = do
      t <- liftIO getCurrentTime
      pure $ toFN (fromUTCTime . (fromMaybe t) .  _time $ v) n v [toLink gn e]


initGridRoot :: KbtzName -> IO (Bool)
initGridRoot name = do
  t <- fromUTCTime <$> getCurrentTime
  let root = getGridRoot name
  a <- withSpider stakeConfig (\s -> addFN s $ toFN t root initSM [])
  b <- withSpider statusConfig (\s -> addFN s $ toFN t root initSM [])
  c <- withSpider meshConfig (\s -> addFN s $ toFN t root initMeshNode [])
  return $ a && b && c
  
  

addMonNode :: forall m. (MonadAsync m, MonadCatch m)
  => KbtzRoot NodeMAC -> FL.Fold m (NodeMAC, (SensorS, Stake, TransactionStatus)) Bool
addMonNode (KbtzRoot gn) = spiderFold statusConfig (\(n, (s, _, st)) -> x n s st)
  where
    x :: NodeMAC
      -> SensorS
      -> TransactionStatus
      -> m (FoundNode NodeMAC SensorS TransactionStatus)
    x n v e = do
      t <- liftIO getCurrentTime
      pure $ toFN (fromUTCTime . (fromMaybe t) .  _time $ v) n v [toLink gn e]


saveTx ::  forall m. (MonadAsync m, MonadCatch m)
  => KbtzRoot NodeMAC -> FL.Fold m (NodeMAC, (SensorS, Stake, TransactionStatus)) (Bool, Bool)
saveTx k = (,) <$> (addStakeNode k) <*> (addMonNode k)

writeSpiderStream :: (IsStream t, MonadAsync m, MonadCatch m)
  => Config n v e
  -> (Spider n v e -> a -> m b)
  -> t m a
  -> t m b
writeSpiderStream conf f as = S.bracket
  (liftIO $ connectWith conf)
  (liftIO . close)
  (\s -> S.mapM (\x -> (liftIO . print $ "writing to spider") >>
                  f s x) as)

getSnapshotStream :: (IsStream t, MonadAsync m, MonadCatch m)
  => Config n v e
  -> (Spider n v e -> m (SnapshotGraph n v e))
  -> t m (SnapshotGraph n v e)
getSnapshotStream conf f = S.bracket
  (liftIO $ connectWith conf)
  (liftIO . close)
  (\s -> S.repeatM $ f s)

type SnapshotId n = (FromGraphSON n, ToJSON n, Ord n, Hashable n, Show n)

getGridSnapshot :: forall m n v e. (SnapshotId n, SpiderConn n v e, MonadIO m)
  => KbtzRoot n
  -> UTCTime
  -> UTCTime
  -> Spider n v e
  -> m (SnapshotGraph n v e)
getGridSnapshot r t t' s = liftIO
                           . (getSnapshot s)
                           . (rangeQuery t t') $ [getRoot r]


getNodesSnapshot :: forall m n v e. (SnapshotId n, SpiderConn n v e, MonadIO m)
  => [n]
  -> UTCTime
  -> UTCTime
  -> Spider n v e
  -> m (SnapshotGraph n v e)
getNodesSnapshot ns t t' s = liftIO
                           . (getSnapshot s)
                           . (rangeQuery t t') $ ns

rangeQuery :: (Eq n, Show n) => UTCTime -> UTCTime -> [n] -> Query n na sla sla
rangeQuery t t' ns = (defQuery ns)
  { timeInterval =
      Finite (fromUTCTime t)
      <=..<=
      Finite (fromUTCTime t')
  , foundNodePolicy = policyAppend } 


getGridSnapshotSimple :: forall m n v e. (SnapshotId n, SpiderConn n v e)
  => KbtzRoot n
  -> Config n v e
  -> IO (SnapshotGraph n v e)
getGridSnapshotSimple r c = withSpider c (\s -> getSnapshotSimple s $ getRoot r) 

statusSnapshot' k = getGridSnapshotSimple k statusConfig

stakeSnapshot' k = getGridSnapshotSimple k stakeConfig

meshSnapshot' n = withSpider meshConfig (\s -> getSnapshotSimple s n)

meshSnapshot ns t0 tn = withSpider meshConfig (getNodesSnapshot ns t0 tn)

stakeSnapshot ns t0 tn = withSpider stakeConfig (getNodesSnapshot ns t0 tn)

statusSnapshot ns t0 tn = withSpider statusConfig (getNodesSnapshot ns t0 tn)



subscribeSnapshot :: forall t m v e.
  (IsStream t, MonadAsync m, MonadCatch m, SpiderConn () v e)
  => KbtzName
  -> Config NodeMAC v e
  -> t m (SnapshotGraph NodeMAC v e)
subscribeSnapshot k c = getSnapshotStream c (\s -> liftIO $ getSnapshotSimple s (getRoot . mkKbtzRoot $ k))


stakeConfig :: Config NodeMAC SensorS Stake
stakeConfig = defConfig { nodeIdKey = "@stake_node" }

statusConfig :: Config NodeMAC SensorS TransactionStatus
statusConfig = defConfig { nodeIdKey = "@status_node"}

meshConfig :: Config NodeMAC MeshNode RxSignal
meshConfig = defConfig { nodeIdKey = "@mesh_node"}


spiderPool :: forall n v e. Config n v e -> IO (Pool (Spider n v e))
spiderPool c = createPool (connectWith c) close 10 100 10
