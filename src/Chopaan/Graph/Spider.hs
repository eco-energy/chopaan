{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes, ConstraintKinds, KindSignatures, QuantifiedConstraints, MultiParamTypeClasses, GADTs, FlexibleInstances#-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns, NoMonomorphismRestriction  #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveAnyClass, DeriveFunctor, StandaloneDeriving, UndecidableInstances, TypeOperators, AllowAmbiguousTypes #-}

{-# LANGUAGE ExplicitForAll, FlexibleContexts, TupleSections, TypeInType #-}
module Chopaan.Graph.Spider where

import Control.Arrow
import qualified Streamly.Prelude as S
import Streamly as S
import qualified Streamly.Internal.Data.Fold as FL

import Data.Proxy
import Data.Map (Map)
import qualified Data.Map as M
import Data.Aeson (ToJSON, FromJSON)
import Data.Text
import Data.Greskell
import Data.Hashable (Hashable)
import Data.Bifunctor
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime(..), getCurrentTime, fromGregorian, secondsToDiffTime)
import Data.Pool

import GHC.Generics
import Control.DeepSeq
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader hiding (ask)
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Monad.Catch

import Chopaan.Node.NodeId
import Chopaan.Node.Metrics hiding (Timestamp)
import Chopaan.Node.Folds
import Chopaan.Node.Mesh (MeshNode, RxSignal, rsToFN, initMeshNode)
import qualified Proto.NodeMessageSchema.NodeMessages as N

import Chopaan.Utils.Retry
import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.Kibbutz
import Chopaan.Kibbutz.Transactor ( TxStatus
                                  , Stake
                                  , Tx(..)
                                  , NodeStates
                                  , TxPlan
                                  , TxState
                                  , stakeLinkDir
                                  , txStatusLinkDir
                                  )
import Chopaan.Graph.Greskell

import NetSpider.Spider (Spider, addFoundNode, getSnapshot, getSnapshotSimple, connectWith, close, withSpider)
import NetSpider.Spider.Config (Config(..), defConfig, LogLevel(..))
import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..), VNode(..))
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Timestamp (fromUTCTime, now, Timestamp)
import Chopaan.Graph.Snapshot (SnapshotGraph, fromNSGraph)
import NetSpider.Query (defQuery, Query(..), Extended(..), (<=..<=), policyAppend)


import Data.String

import Chopaan.Graph.G

type (~>) f g = forall x. f x -> g x

type Spool n = SpoolG n

type Spools = SpG'' NodeMAC

type SnapshotId n = (FromGraphSON n, ToJSON n, Ord n, Hashable n, Show n)

mkSpool :: forall m n. (MonadIO m, SnapshotId n) => ConfG n -> m (SpG'' n)
mkSpool (G''{meshG, txG, statusG, flowG}) = G''
                                        <$> (SpoolG <$> (spiderPool (unConf meshG)))
                                        <*> (SpoolG <$> spiderPool (unConf txG))
                                        <*> (SpoolG <$> spiderPool (unConf statusG))
                                        <*> (SpoolG <$> spiderPool (unConf flowG))



runSpider :: (MonadIO m) => Spools -> SpiderM ~> m
runSpider c a = liftIO $ runReaderT (runSpiderM a) c

newtype SpiderM a = SpiderM { runSpiderM :: ReaderT (Spools) IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO,
                    MonadThrow, MonadCatch, MonadReader (Spools),
                    MonadBase IO, MonadBaseControl IO)


type SpiderConn n v e = (SpiderNodeId n, NodeAttributes v, LinkAttributes e, Show n, Show v, Show e)

type SpiderNodeId n = (ToJSON n)

newtype KbtzRoot n = KbtzRoot { getRoot :: n }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

mkKbtzRoot :: KbtzName -> KbtzRoot NodeMAC
mkKbtzRoot (KbtzId k) = (KbtzRoot (NodeId ("grid_" <> k) :: NodeMAC))
{-# INLINE mkKbtzRoot #-}

getGridRoot :: KbtzName -> NodeMAC
getGridRoot = getRoot . mkKbtzRoot
{-# INLINE getGridRoot #-}

class (NodeAttributes v) => HasTime v where
  getVTime :: v -> Maybe UTCTime

instance (GreskellC e, GreskellC p) => HasTime (SensorMetrics e p) where
  getVTime = _time

class (LinkAttributes e) => HasDir e where
  getEDir :: e -> LinkState

instance HasDir Stake where
  getEDir = stakeLinkDir

instance HasDir TxStatus where
  getEDir = txStatusLinkDir

instance (GreskellC p, Num p, Ord p) => HasDir (Node p) where
  getEDir Node{tx}
    | tx > 0 = LinkToSubject
    | tx < 0 = LinkToTarget
    | tx == 0 = LinkBidirectional
    | otherwise = LinkUnused
    
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
addFN s f = expToBool =<< (liftIO $ -- (print $ neighborLinks f) >>
                           (try (addFoundNode s f)))
{-# INLINE addFN #-}


tryForBool :: (MonadIO m, MonadCatch m) => m a -> m Bool 
tryForBool m = expToBool =<< (try m)
{-# INLINE tryForBool #-}

expToBool :: (MonadIO m) => Either SomeException a -> m Bool
expToBool (Left e) = (liftIO . print $ e) >> return False
expToBool (Right _) = return True
{-# INLINE expToBool #-}


toFN :: (SpiderConn n v e) => Timestamp -> n -> v -> [FoundLink n e] -> FoundNode n v e
toFN t n v lx = FoundNode
      { subjectNode = n
      , foundAt = t 
      , neighborLinks = lx
      , nodeAttributes = v
      }
{-# INLINE toFN #-}


toLink :: HasDir e => n -> e -> FoundLink n e
toLink n' e = FoundLink
                     { targetNode = n'
                     , linkState = getEDir e
                     , linkAttributes = e
                     }
{-# INLINE toLink #-}

toLink' :: n -> e -> LinkState -> FoundLink n e
toLink' n' e dir = FoundLink
                     { targetNode = n'
                     , linkState = dir
                     , linkAttributes = e
                     }
{-# INLINE toLink' #-}

spiderFold :: forall m a n v e. (MonadAsync m, MonadCatch m, SpiderConn n v e)
           => Pool (Spider n v e)
           -> ((n, a) -> m (Maybe (FoundNode n v e)))
           -> FL.Fold m (n, a) Bool
spiderFold p asFN = FL.mkFold step (pure True) end
  where
    {-# INLINE step #-}
    step :: (Bool)
         -> (n, a)
         -> m Bool
    step _ a = addMaybe =<< (asFN a)
      where
        addMaybe Nothing = return True
        addMaybe (Just n) = withResource p ((flip addFN) n)
    end x = liftIO $ destroyAllResources p >> return x
{-# INLINE spiderFold #-}

--utcToRange :: UTCTime -> UTCTime -> _
utcToRange t t' = Finite (fromUTCTime t) <=..<= Finite (fromUTCTime t')
{-# INLINE utcToRange #-}


infRange = NegInf <=..<= PosInf
{-# INLINE infRange #-}

rangeQuery :: (Eq n, Show n) => UTCTime -> UTCTime -> [n] -> Query n na sla sla
rangeQuery t t' ns = (defQuery ns)
  { timeInterval = utcToRange t t'
  , foundNodePolicy = policyAppend
  --, includeIncomingLinks = True
  } 
{-# INLINE rangeQuery #-}

class HasLabel a where
  label :: a -> Text
  nodeType :: a -> Proxy v
  edgeType :: a -> Proxy e


instance HasLabel (G k n) where
  label (Mesh _) = "mesh"
  label (Transactor _) = "transactor"
  label (Status _) = "status"
  label (Flow _) = "flow"

toKey :: forall n. Text -> Key VNode n 
toKey a = fromString . unpack $ "@" <> a <> "_node"
{-# INLINE toKey #-}

type Opts = (String, Int)


hasConfig :: forall a n v e. (SpiderConn n v e)
  => Opts -> Text -> Config n v e
hasConfig (h, p) label = defConfig
  { wsHost = h
  , wsPort = p
  , nodeIdKey = toKey label
  , logThreshold = LevelWarn
  }
{-# INLINE hasConfig #-}

spiderPool :: forall m n v e. MonadIO m => Config n v e -> m (Pool (Spider n v e))
spiderPool c = liftIO $ createPool
  ((recoverC "retrying kbtz janusgraph connection" 100) (connectWith c)) close 10 100 10


fromNSGraphM = (pure . fromNSGraph)

gridSnapshotSimple :: forall m n v e. (MonadIO m, SpiderConn n v e)
  => KbtzName
  -> Spider NodeMAC v e
  -> m (SnapshotGraph NodeMAC v e)
gridSnapshotSimple k s = liftIO $
                         fromNSGraphM =<< (getSnapshotSimple s $ getGridRoot k)
{-# INLINE gridSnapshotSimple #-}


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



subscribeSnapshot :: forall t m v e.
  (IsStream t, MonadAsync m, MonadCatch m, SpiderConn () v e)
  => KbtzName
  -> Config NodeMAC v e
  -> t m (SnapshotGraph NodeMAC v e)
subscribeSnapshot k c = getSnapshotStream c (\s ->
                                               liftIO $ fromNSGraphM
                                               =<< (getSnapshotSimple s (getRoot . mkKbtzRoot $ k)))



initGridRoot :: KbtzName -> [NodeMAC] -> SpiderM (Bool)
initGridRoot name ns = do
  spool <- ask
  --t <- fromUTCTime <$> getCurrentTime
  let t = fromUTCTime $ UTCTime (fromGregorian 2021 4 6) (secondsToDiffTime 0)
  let root = getGridRoot name
  --a <- withResource (unSpool . txG $ spool) (\s -> addFN s $ toFN t root mempty [])
  --b <- withResource (unSpool . statusG $ spool) (\s -> addFN s $ toFN t root initSM [])
  --c <- withResource (unSpool . meshG $ spool) (\s -> addFN s $ toFN t root initMeshNode [])
  -- print =<< (gridSnapshotSimple name meshConfig)
  -- print =<< (gridSnapshotSimple name stakeConfig)
  -- print =<< (gridSnapshotSimple name statusConfig)
  return $ True -- a && b && c
  -- (foldl (&&) True a) && (foldl (&&) True b) && (foldl (&&) True c)
  where
    initialEdges :: forall e. (Monoid e, LinkAttributes e, HasDir e) => [FoundLink NodeMAC e]
    initialEdges = ((flip toLink $ mempty) <$> ns)
    reverseFN :: forall v e. (Show v, Show e, NodeAttributes v, LinkAttributes e, Monoid e)
              => v -> NodeMAC -> Timestamp -> NodeMAC -> FoundNode NodeMAC v e 
    reverseFN v root t n = toFN t n v [toLink' root mempty LinkBidirectional]
    reverseFNs :: forall v e. (Show v, Show e, NodeAttributes v, LinkAttributes e, Monoid e)
              => v -> NodeMAC -> Timestamp -> [FoundNode NodeMAC v e]
    reverseFNs v root t = (reverseFN v root t) <$> ns
    meshEdges :: [FoundLink NodeMAC RxSignal]
    meshEdges = ((\n -> toLink' n mempty LinkBidirectional) <$> ns)
  


addMeshNode :: (MonadAsync m, MonadCatch m) => SpiderM (FL.Fold m (NodeMAC, N.RuntimeStats) Bool)
addMeshNode = do
  spool <- ask
  return $ spiderFold (unSpool . meshG $ spool) (pure . Just . rsToFN)

addTxNode :: forall m. (MonadAsync m, MonadCatch m)
  => KbtzName -> SpiderM (FL.Fold m (NodeMAC, (SensorR, Maybe Stake, Maybe TxStatus)) Bool)
addTxNode k = do
  spool <- ask
  return $ spiderFold (unSpool . txG $ spool)
    (\(n, (s, stake, status)) -> case stake of
        Nothing -> pure Nothing
        Just stk -> case status of
          Nothing -> pure Nothing
          Just (stts) -> Just <$> (x (_time s) n stk stts))
  where
    x :: Maybe UTCTime -> NodeMAC -> Stake -> TxStatus -> m (FoundNode NodeMAC Stake TxStatus)
    x t n v e = do
      t' <- liftIO getCurrentTime
      pure $ toFN (fromUTCTime . (fromMaybe t') $ t) n v [toLink (getGridRoot k) e]


addFlowNode :: forall m. (MonadAsync m, MonadCatch m)
  => KbtzName -> SpiderM (FL.Fold m (NodeMAC, (SensorR, Maybe Stake, Maybe TxStatus)) Bool)
addFlowNode k = do
  spool <- ask
  return $ spiderFold (unSpool . flowG $ spool)
    (\(n, (s, stake, status)) -> Just <$> (x (_time s) n s))
  where
    x :: Maybe UTCTime -> NodeMAC -> SensorR -> m (FoundNode NodeMAC BatteryR PowerNR)
    x t n v = do
      t' <- liftIO getCurrentTime
      pure $ toFN (fromUTCTime . (fromMaybe t') $ t) n (_battery v)
        [toLink (getGridRoot k) (_powerT v)]


gridState :: KbtzName -> NodeStates n -> TxPlan n -> TxState n -> IO (Bool)
gridState k = undefined


addMonNode :: forall m. (MonadAsync m, MonadCatch m)
  => KbtzName -> SpiderM (FL.Fold m (NodeMAC, (SensorR, Maybe Stake, Maybe TxStatus)) Bool)
addMonNode k = do
  spool <- ask
  return $ spiderFold (unSpool . statusG $ spool) (\(n, (s, st, _)) -> x n s st)
  where
    x :: NodeMAC
      -> SensorR
      -> Maybe (Stake)
      -> m (Maybe (FoundNode NodeMAC SensorR Stake))
    x n v e = case e of
                Nothing -> return Nothing
                Just stk -> do
                  t <- liftIO getCurrentTime
                  pure . Just $
                    toFN (fromUTCTime . (fromMaybe t) .  _time $ v) n v [toLink (getGridRoot k) stk]


saveTx ::  forall m. (MonadAsync m, MonadCatch m)
  => KbtzName
  -> SpiderM (FL.Fold m (NodeMAC, (SensorR, Maybe Stake, Maybe TxStatus)) (Bool))
saveTx k = do
  stakeF <- addTxNode k
  monF <- addMonNode k
  flowF <- addFlowNode k
  return $ (\(a, b, c) -> a && b && c) <$> ((,,) <$> stakeF <*> monF <*> flowF)

-- saveTx' ::  forall m. (MonadAsync m, MonadCatch m)
--   => KbtzName
--   -> SpiderM (FL.Fold m (NodeMAC,
--                          (NodeStates NodeMAC, Maybe (TxPlan NodeMAC), TxState NodeMAC))
--                (Bool, Bool))
-- saveTx' k = do
--   stakeF <- addTxNode k
--   monF <- addMonNode k
--   return $ (\(x, y) -> (truthFold x, truthFold y))
--     <$> ((,) <$> (FL.lmap  stakeF) <*> (FL.classify monF))
--   where
--     truthFold = (Prelude.foldl (&&) True)


gridSnapshot :: forall m v e. (SpiderConn NodeMAC v e, MonadIO m)
  => KbtzName
  -> UTCTime
  -> UTCTime
  -> Spider NodeMAC v e
  -> m (SnapshotGraph NodeMAC v e)
gridSnapshot r t t' s = fromNSGraphM
                        =<< (liftIO
                           . (getSnapshot s)
                           . (rangeQuery t t') $ [getGridRoot r])


nodesSnapshot :: forall m n v e. (SnapshotId n, SpiderConn n v e, MonadAsync m)
  => [n]
  -> UTCTime
  -> UTCTime
  -> Pool (Spider n v e)
  -> m (SnapshotGraph n v e)
nodesSnapshot ns t t' p = liftIO $ withResource p (\s ->
                                                     fromNSGraphM
                                                     =<< (getSnapshot s $ rangeQuery t t' ns))
{-# INLINE nodesSnapshot #-}

statusGridSnapshot :: KbtzName
  -> UTCTime
  -> UTCTime
  -> SpiderM (SnapshotGraph NodeMAC SensorR Stake)
statusGridSnapshot k t t' = do
  spool <- ask
  liftIO $ withResource (unSpool . statusG $ spool) (gridSnapshot k t t')

txGridSnapshot :: KbtzName
  -> UTCTime
  -> UTCTime
  -> SpiderM (SnapshotGraph NodeMAC Stake TxStatus)
txGridSnapshot k t t' = do
  spool <- ask
  liftIO $ withResource (unSpool . txG $ spool) (gridSnapshot k t t')

meshGridSnapshot :: KbtzName
  -> UTCTime
  -> UTCTime
  -> SpiderM (SnapshotGraph NodeMAC MeshNode RxSignal)
meshGridSnapshot k t t' = do
  spool <- ask
  liftIO $ withResource (unSpool . meshG $ spool) (gridSnapshot k t t')

flowGridSnapshot :: KbtzName
  -> UTCTime
  -> UTCTime
  -> SpiderM (SnapshotGraph NodeMAC BatteryR PowerNR)
flowGridSnapshot k t t' = do
  spool <- ask
  liftIO $ withResource (unSpool . flowG $ spool) (gridSnapshot k t t')


statusNodesSnapshot :: [NodeMAC]
  -> UTCTime
  -> UTCTime
  -> SpiderM (SnapshotGraph NodeMAC SensorR Stake)
statusNodesSnapshot k t t' = ((nodesSnapshot k t t') . (unSpool . statusG)) =<< ask

txNodesSnapshot :: [NodeMAC]
  -> UTCTime
  -> UTCTime
  -> SpiderM (SnapshotGraph NodeMAC Stake TxStatus)
txNodesSnapshot k t t' = ((nodesSnapshot k t t') . (unSpool . txG)) =<< ask

meshNodesSnapshot :: [NodeMAC]
  -> UTCTime
  -> UTCTime
  -> SpiderM (SnapshotGraph NodeMAC MeshNode RxSignal)
meshNodesSnapshot k t t' = ((nodesSnapshot k t t') . (unSpool . meshG)) =<< ask

flowNodesSnapshot :: [NodeMAC]
  -> UTCTime
  -> UTCTime
  -> SpiderM (SnapshotGraph NodeMAC BatteryR PowerNR)
flowNodesSnapshot k t t' = ((nodesSnapshot k t t') . (unSpool . flowG)) =<< ask

mkConfG :: Opts -> ConfG NodeMAC
mkConfG o = G'' (CG $ meshConfig o) (CG $ txConfig o) (CG $ statusConfig o) (CG $ flowConfig o) 

txConfig :: Opts -> Config NodeMAC Stake TxStatus
txConfig o = hasConfig o "transactor"

statusConfig :: Opts -> Config NodeMAC SensorR Stake
statusConfig o = hasConfig o "status"

meshConfig :: Opts -> Config NodeMAC MeshNode RxSignal
meshConfig o = hasConfig o "mesh"

flowConfig :: Opts -> Config NodeMAC BatteryR PowerNR
flowConfig o = hasConfig o "flow"
