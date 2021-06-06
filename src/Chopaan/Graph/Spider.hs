{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes, ConstraintKinds, KindSignatures, QuantifiedConstraints, MultiParamTypeClasses, GADTs, FlexibleInstances#-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns, NoMonomorphismRestriction  #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveAnyClass, DeriveFunctor, StandaloneDeriving #-}

{-# LANGUAGE ExplicitForAll, FlexibleContexts #-}
module Chopaan.Graph.Spider where

import qualified Streamly.Prelude as S
import Streamly.Prelude
import qualified Streamly.Internal.Data.Fold as FL

import GHC.Generics
import Control.DeepSeq
import Control.Monad.IO.Class
import Control.Monad.Catch

import Chopaan.Comm.Address
import Chopaan.Node.NodeId
import Chopaan.Node.Metrics hiding (TimeStamp, Timestamp)
import Chopaan.Node.Folds
import Chopaan.Node.Mesh (MeshNode, RxSignal)

import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.Kibbutz
import Chopaan.Kibbutz.Transactor (planTx
                                  , monitorTx
                                  , dispatchTx
                                  , TransactionStatus
                                  , Stake
                                  , Role(..)
                                  , curryTx
                                  , stakeLinkDir
                                  , txStatusLinkDir
                                  )
import Chopaan.Graph.Greskell

import NetSpider.Spider
import NetSpider.Spider (Spider, addFoundNode, getSnapshot, getSnapshotSimple, connectWith, close)
import NetSpider.Spider.Config (Config(..), defConfig)
import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..))
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Timestamp (fromUTCTime, now, Timestamp)
import NetSpider.Snapshot (SnapshotGraph)
import NetSpider.Query (defQuery, Query(..), Extended(..), (<=..<=))


import Data.Aeson (ToJSON, FromJSON)
import Data.Greskell
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)


import Control.Monad.Trans.Reader



type SpiderConn n v e = (SpiderNodeId n, NodeAttributes v, LinkAttributes e)

type SpiderM m n c d = ReaderT (Spider n c d) m


type SpiderNodeId n = (ToJSON n)

newtype KbtzRoot n = KbtzRoot { getRoot :: n }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

mkKbtzRoot :: KbtzName -> KbtzRoot NodeMAC
mkKbtzRoot (KbtzId k) = (KbtzRoot (NodeId ("grid_" <> k) :: NodeMAC))

getGridRoot :: KbtzName -> NodeMAC
getGridRoot = getRoot . mkKbtzRoot


-- spiderF :: forall m n a c d. (MonadAsync m, Address n)
--   => Config n c d
--   -> (Spider n c d -> (n, a) -> m ())
--   -> FL.Fold (SpiderM m n c d) (n, a) ()
-- spiderF c f = FL.Fold (step) i o
--   where
--     step arg i' = do
--       s <- ask
--       return (f s arg i')
--     i = mkSpider c
--     o = pure
  

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
      liftIO . print $ "adding a node"
      t' <- liftIO now
      let
        t = fromMaybe t' $ fromUTCTime <$> (getVTime v)
        lx = (toLink getEDir) gn <$> es
      expToBool =<< (liftIO $ try (addFoundNode spider $ toFN t (n, v) lx)) 
    
expToBool :: (MonadIO m) => Either SomeException () -> m Bool
expToBool (Left e) = (liftIO . print $ e) >> return False
expToBool (Right _) = return True
    
toFN :: (SpiderConn n v e) => Timestamp -> (n, v) -> [FoundLink n e] -> FoundNode n v e
toFN t (n, v) lx = FoundNode
      { subjectNode = n
      , foundAt = t 
      , neighborLinks = lx
      , nodeAttributes = v
      }

toLink :: (e -> LinkState) -> n -> e -> FoundLink n e
toLink getDir n' e = FoundLink
                     { targetNode = n'
                     , linkState = getDir e
                     , linkAttributes = e
                     }

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
  -> Spider n v e
  -> UTCTime
  -> UTCTime
  -> m (SnapshotGraph n v e)
getGridSnapshot r s t t' = liftIO . (getSnapshot s) . (mkQuery . getRoot) $ r
  where
    mkQuery gridRoot = (defQuery [gridRoot]) {
      timeInterval =
        Finite (fromUTCTime t)
        <=..<=
        Finite (fromUTCTime t')
      } 


subscribeSnapshot :: forall t m v e.
  (IsStream t, MonadAsync m, MonadCatch m, SpiderConn () v e)
  => KbtzName
  -> Config NodeMAC v e
  -> t m (SnapshotGraph NodeMAC v e)
subscribeSnapshot k c = getSnapshotStream c (\s -> liftIO $ getSnapshotSimple s (getRoot . mkKbtzRoot $ k))


stakeConfig :: Config NodeMAC SensorS Stake
stakeConfig = defConfig

statusConfig :: Config NodeMAC SensorS TransactionStatus
statusConfig = defConfig

meshConfig :: Config NodeMAC MeshNode RxSignal
meshConfig = defConfig
