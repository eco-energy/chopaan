{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes, ConstraintKinds, KindSignatures, QuantifiedConstraints, MultiParamTypeClasses, GADTs, FlexibleInstances#-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns, NoMonomorphismRestriction  #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveAnyClass, DeriveFunctor, StandaloneDeriving, UndecidableInstances, TypeOperators, AllowAmbiguousTypes #-}

{-# LANGUAGE ExplicitForAll, FlexibleContexts, TupleSections, TypeInType #-}
module Chopaan.Graph.Spider where

import Streamly.Prelude (IsStream, MonadAsync)
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold.Tee as FL

import Data.Proxy
import Data.Aeson (ToJSON, FromJSON)
import Data.Text
import Data.Greskell
import Data.Hashable (Hashable)
import Data.Bifunctor ()
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime(..), getCurrentTime)
import Data.Time.Compat (secondsToNominalDiffTime)
import Data.Pool

import GHC.Generics
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader hiding (ask)
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Unlift

import Chopaan.Node.NodeId
import Chopaan.Node.Metrics hiding (Timestamp)
import Chopaan.Node.Folds
import Chopaan.Node.Mesh (MeshNode, RxSignal, sigToFN)
import Chopaan.Types (PoolConf(..))
import Chopaan.Utils.Retry
import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.Kibbutz
import Chopaan.Kibbutz.Transactor ( TxStatus
                                  , Stake
                                  , NodeStates
                                  , TxPlan
                                  , TxState
                                  , stakeLinkDir
                                  , txStatusLinkDir
                                  )
import Chopaan.Graph.Greskell

import qualified Network.Greskell.WebSocket.Client.Options as Gr
import qualified Network.Greskell.WebSocket.Client as Gr
import NetSpider.Spider (Spider(..), addFoundNode, getSnapshot, getSnapshotSimple, connectWith, connectWithOpts, close)
import NetSpider.Spider.Config (Config(..), defConfig, LogLevel(..))
import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..), VNode)
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

  
spiderPool :: forall m n v e. MonadIO m => PoolConf -> Config n v e -> m (Pool (Spider n v e))
spiderPool pc c = liftIO $ createPool mkConn close (pNumStripes pc) (secondsToNominalDiffTime . realToFrac . reaperWait $ pc) (maxConnsPerStripe pc)
  where
    mkConn = ((recoverC "retrying kbtz janusgraph connection" 10) (connectWith c))

monitorSpool :: SpG'' n -> IO ()
monitorSpool (G''{meshG, txG, statusG, flowG}) = do
  pwint meshG
  pwint txG
  pwint statusG
  pwint flowG
  where
    pwint s = (print . poolStats) =<< ((flip stats $ True) . unSpool $ s)

    
mkSpool :: forall m n. (MonadIO m, SnapshotId n) => PoolConf -> ConfG n -> m (SpG'' n)
mkSpool pc (G''{meshG, txG, statusG, flowG}) = G''
                                               <$> (SpoolG <$> (spiderPool pc (unConf meshG)))
                                               <*> (SpoolG <$> spiderPool pc (unConf txG))
                                               <*> (SpoolG <$> spiderPool pc (unConf statusG))
                                               <*> (SpoolG <$> spiderPool pc (unConf flowG))



runSpider :: (MonadIO m) => Spools -> SpiderM ~> m
runSpider c a = liftIO $ runReaderT (runSpiderM a) c
{-# INLINE runSpider #-}

newtype SpiderM a = SpiderM { runSpiderM :: ReaderT (Spools) IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO,
                    MonadThrow, MonadCatch, MonadReader (Spools),
                    MonadBase IO, MonadBaseControl IO, MonadUnliftIO)


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
      expToBool =<< (addFN spider $ toFN t n v lx) 

data SpiderException = AddFNExp SomeException
  deriving (Generic, Show, Exception)

addFN :: MonadIO m
      => MonadCatch m
      => SpiderConn n v e
      => Spider n v e -> FoundNode n v e -> m (Either SpiderException Bool) 
addFN s f = toE =<<  (liftIO $ (try (addFoundNode s f)))
  where
    toE (Left err) = (liftIO . print $ err) >> (return (Left . AddFNExp $ err))
    toE (Right yay) = (liftIO . print $ yay) >> (return (Right $ True)) 
{-# INLINE addFN #-}


expToBool :: (MonadIO m, Exception e) => Either e a -> m Bool
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


addFNMaybe :: forall m n v e. (MonadAsync m, MonadCatch m, SpiderConn n v e)
           => Pool (Spider n v e) -> Maybe (FoundNode n v e) -> m (Bool)
addFNMaybe _ Nothing = return True
addFNMaybe p (Just n) = expToBool =<< withResourceOnEither p ((flip addFN) n)
{-# INLINE addFNMaybe #-}

addFNE :: forall m n v e. (MonadAsync m, MonadCatch m, SpiderConn n v e)
           => Pool (Spider n v e) -> Maybe (FoundNode n v e) -> m (Either SpiderException Bool)
addFNE _ Nothing = return (Right True)
addFNE p (Just n) = withResourceOnEither p ((flip addFN) n)
{-# INLINE addFNE #-}

spiderFold :: forall m a n v e. (MonadAsync m, MonadCatch m, SpiderConn n v e)
           => Pool (Spider n v e)
           -> ((n, a) -> m (Maybe (FoundNode n v e)))
           -> FL.Fold m (n, a) Bool
spiderFold p asFN = FL.foldlM' (\_ -> addFNMaybe p <=< asFN) (pure True)
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


withResourceOnEither :: (MonadIO m) => Pool resource -> (resource -> IO (Either failure success)) -> m (Either failure success)
withResourceOnEither pool act = liftIO $ mask_ $ do
  (resource, localPool) <- takeResource pool
  failureOrSuccess <- act resource `onException` destroyResource pool localPool resource
  case failureOrSuccess of
    Right success -> do
      putResource localPool resource
      return (Right success)
    Left failure -> do
      destroyResource pool localPool resource
      return (Left failure)
{-# INLINE withResourceOnEither #-}

fromNSGraphM = (pure . fromNSGraph)
{-# INLINE fromNSGraphM #-}

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
{-# INLINE writeSpiderStream #-}

getSnapshotStream :: (IsStream t, MonadAsync m, MonadCatch m)
  => Config n v e
  -> (Spider n v e -> m (SnapshotGraph n v e))
  -> t m (SnapshotGraph n v e)
getSnapshotStream conf f = S.bracket
  (liftIO $ connectWith conf)
  (liftIO . close)
  (\s -> S.repeatM $ f s)
{-# INLINE getSnapshotStream #-}


subscribeSnapshot :: forall t m v e.
  (IsStream t, MonadAsync m, MonadCatch m, SpiderConn () v e)
  => KbtzName
  -> Config NodeMAC v e
  -> t m (SnapshotGraph NodeMAC v e)
subscribeSnapshot k c = getSnapshotStream c (\s ->
                                               liftIO $ fromNSGraphM
                                               =<< (getSnapshotSimple s (getRoot . mkKbtzRoot $ k)))
{-# INLINE subscribeSnapshot #-}

addMeshNode :: (MonadAsync m, MonadCatch m)
  => SpiderM (FL.Fold m (NodeMAC, (MeshNode, RxSignal)) Bool)
addMeshNode = do
  spool <- ask
  return $ spiderFold (unSpool . meshG $ spool) (pure . Just . sigToFN)
{-# INLINE addMeshNode #-}

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
    {-# INLINE x #-}
{-# INLINE addTxNode #-}

flowFN :: (MonadAsync m, MonadCatch m) => KbtzName -> NodeMAC -> SensorR -> m (FoundNode NodeMAC BatteryR PowerNR)
flowFN k n v = do
  t' <- liftIO getCurrentTime
  let t = (fromUTCTime . (fromMaybe t') $ (_time v))
  pure $
    toFN t n (_battery v) [toLink (getGridRoot k) (_powerT v)]
{-# INLINE flowFN #-}

addFlow :: KbtzName -> (NodeMAC, SensorR) -> SpiderM (Either SpiderException Bool)
addFlow k (n, v) = do
  spool <- ask
  fn <- flowFN k n v
  addFNE (unSpool . flowG $ spool) (Just fn)
{-# INLINE addFlow #-}

addMeshN :: (NodeMAC, (MeshNode, RxSignal)) -> SpiderM (Either SpiderException Bool)
addMeshN (v, l) = do
  spool <- ask
  let fn = sigToFN (v, l)
  addFNE (unSpool . meshG $ spool) (Just $ fn)
{-# INLINE addMeshN #-}

addTx :: KbtzName -> (NodeMAC, (SensorR, Maybe Stake, Maybe TxStatus)) -> SpiderM (Either SpiderException Bool)
addTx k (n, (s, stake, status)) = do
  spool <- ask
  let stake' = fromMaybe mempty stake
      status' = fromMaybe mempty status
  fn <- Just <$> (x (_time s) n stake' status')
  addFNE (unSpool . txG $ spool) fn
  where
    x :: Maybe UTCTime -> NodeMAC -> Stake -> TxStatus -> SpiderM (FoundNode NodeMAC Stake TxStatus)
    x t n v e = do
      t' <- liftIO getCurrentTime
      pure $ toFN (fromUTCTime . (fromMaybe t') $ t) n v [toLink (getGridRoot k) e]
    {-# INLINE x#-}
{-# INLINE addTx #-}

addMon :: KbtzName -> (NodeMAC, (SensorR, Maybe Stake)) -> SpiderM (Either SpiderException Bool)
addMon k (n, (s, st)) = do
  spool <- ask
  fn <- x n s st
  addFNE (unSpool . statusG $ spool) fn
  where
    x :: NodeMAC
      -> SensorR
      -> Maybe (Stake)
      -> SpiderM (Maybe (FoundNode NodeMAC SensorR Stake))
    x n v e = do
      let stake = case e of
                 Nothing -> mempty
                 Just stk -> stk
      t <- liftIO getCurrentTime
      pure . Just $
        toFN (fromUTCTime . (fromMaybe t) .  _time $ v) n v [toLink (getGridRoot k) stake]
    {-# INLINE x #-}
{-# INLINE addMon #-}

addFlowNode :: forall m. (MonadAsync m, MonadCatch m)
  => KbtzName -> SpiderM (FL.Fold m (NodeMAC, SensorR) Bool)
addFlowNode k = do
  spool <- ask
  return $ spiderFold (unSpool . flowG $ spool)
    (\(n, s) -> Just <$> (flowFN k n s))
{-# INLINE addFlowNode #-}


gridState :: KbtzName -> NodeStates n -> TxPlan n -> TxState n -> IO (Bool)
gridState k = undefined


addMonNode :: forall m. (MonadAsync m, MonadCatch m)
  => KbtzName -> SpiderM (FL.Fold m (NodeMAC, (SensorR, Maybe Stake)) Bool)
addMonNode k = do
  spool <- ask
  return $ spiderFold (unSpool . statusG $ spool) (\(n, (s, st)) -> x n s st)
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
    {-# INLINE x #-}
{-# INLINE addMonNode #-}

saveTx ::  forall m. (MonadAsync m, MonadCatch m)
  => KbtzName
  -> SpiderM (FL.Fold m (NodeMAC, (SensorR, Maybe Stake, Maybe TxStatus)) (Bool))
saveTx k = do
  stakeF' <- addTxNode k
  monF' <- addMonNode k
  flowF' <- addFlowNode k
  let flowF = FL.Tee $ FL.lmap (\(n, (a, _, _)) -> (n, a)) flowF'
  let monF = FL.Tee $ FL.lmap (\(n, (a, s, _)) -> (n, (a, s))) monF'
  let stakeF = FL.Tee $ stakeF'
  return $ FL.toFold $ (\(a, b, c) -> a && b && c) <$> ((,,) <$> stakeF <*> monF <*> flowF)
{-# INLINE saveTx #-}


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
{-# INLINE gridSnapshot #-}

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
{-# INLINE statusGridSnapshot #-}

txGridSnapshot :: KbtzName
  -> UTCTime
  -> UTCTime
  -> SpiderM (SnapshotGraph NodeMAC Stake TxStatus)
txGridSnapshot k t t' = do
  spool <- ask
  liftIO $ withResource (unSpool . txG $ spool) (gridSnapshot k t t')
{-# INLINE txGridSnapshot #-}

meshGridSnapshot :: KbtzName
  -> UTCTime
  -> UTCTime
  -> SpiderM (SnapshotGraph NodeMAC MeshNode RxSignal)
meshGridSnapshot k t t' = do
  spool <- ask
  liftIO $ withResource (unSpool . meshG $ spool) (gridSnapshot k t t')
{-# INLINE meshGridSnapshot #-}

flowGridSnapshot :: KbtzName
  -> UTCTime
  -> UTCTime
  -> SpiderM (SnapshotGraph NodeMAC BatteryR PowerNR)
flowGridSnapshot k t t' = do
  spool <- ask
  liftIO $ withResource (unSpool . flowG $ spool) (gridSnapshot k t t')
{-# INLINE flowGridSnapshot #-}

statusNodesSnapshot :: [NodeMAC]
  -> UTCTime
  -> UTCTime
  -> SpiderM (SnapshotGraph NodeMAC SensorR Stake)
statusNodesSnapshot k t t' = ((nodesSnapshot k t t') . (unSpool . statusG)) =<< ask
{-# INLINE statusNodesSnapshot #-}


txNodesSnapshot :: [NodeMAC]
  -> UTCTime
  -> UTCTime
  -> SpiderM (SnapshotGraph NodeMAC Stake TxStatus)
txNodesSnapshot k t t' = ((nodesSnapshot k t t') . (unSpool . txG)) =<< ask
{-# INLINE txNodesSnapshot #-}

meshNodesSnapshot :: [NodeMAC]
  -> UTCTime
  -> UTCTime
  -> SpiderM (SnapshotGraph NodeMAC MeshNode RxSignal)
meshNodesSnapshot k t t' = ((nodesSnapshot k t t') . (unSpool . meshG)) =<< ask
{-# INLINE meshNodesSnapshot #-}


flowNodesSnapshot :: [NodeMAC]
  -> UTCTime
  -> UTCTime
  -> SpiderM (SnapshotGraph NodeMAC BatteryR PowerNR)
flowNodesSnapshot k t t' = ((nodesSnapshot k t t') . (unSpool . flowG)) =<< ask
{-# INLINE flowNodesSnapshot #-}


mkConfG :: Opts -> ConfG NodeMAC
mkConfG o = G'' (CG $ meshConfig o) (CG $ txConfig o) (CG $ statusConfig o) (CG $ flowConfig o) 
{-# INLINE mkConfG #-}

txConfig :: Opts -> Config NodeMAC Stake TxStatus
txConfig o = hasConfig o "transactor"
{-# INLINE txConfig #-}

statusConfig :: Opts -> Config NodeMAC SensorR Stake
statusConfig o = hasConfig o "status"
{-# INLINE statusConfig #-}

meshConfig :: Opts -> Config NodeMAC MeshNode RxSignal
meshConfig o = hasConfig o "mesh"
{-# INLINE meshConfig #-}

flowConfig :: Opts -> Config NodeMAC BatteryR PowerNR
flowConfig o = hasConfig o "flow"
{-# INLINE flowConfig #-}
