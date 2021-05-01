{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes, ConstraintKinds, KindSignatures, QuantifiedConstraints, MultiParamTypeClasses, GADTs, FlexibleInstances#-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns, NoMonomorphismRestriction  #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveAnyClass, DeriveFunctor #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chopaan.Kibbutz (runKibbutz, mkKbtzConf, stakeConfig, meshConfig, statusConfig, getGridRoot) where

import GHC.Generics

import Data.Aeson (ToJSON, FromJSON)
import Data.Greskell
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)

import Chopaan.Types hiding (DBOpts)
import Kbtz

import Control.DeepSeq (NFData)
import Control.Arrow (second)
import ConCat.Misc (result)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.Catch

import qualified Control.Concurrent.Async as A

import Streamly
import qualified Streamly.Prelude as S


import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.Kibbutz

import Chopaan.Comm.Mqtt.AWS (withMqttAuth)
import Chopaan.Kibbutz.AWS.Common (newLogger, LogLevel(..))
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
import Chopaan.Kibbutz.Mesh

import Chopaan.Node.NodeId (NodeId(..), NodeMAC)
import Chopaan.Node.Folds (SensorS)
import Chopaan.Node.Node (nodeS)
import Chopaan.Node.Metrics (SensorMetrics(_time))
import Chopaan.Node.HW

import Proto.NodeMessageSchema.NodeMessages (RuntimeStats, EnergyState)
import Chopaan.Comm.Mqtt (runMqtt)
import Chopaan.Comm.Comm (MessageQs(..)
                         , mkCallback
                         , WriteChan
                         )
import System.IO (stdout)

import NetSpider.Spider (Spider, addFoundNode, getSnapshot, getSnapshotSimple, connectWith, close)
import NetSpider.Spider.Config (Config(..), defConfig)
import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..))
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Timestamp (fromUTCTime, now, Timestamp)
import NetSpider.Snapshot (SnapshotGraph)
import NetSpider.Query (defQuery, Query(..), Extended(..), (<=..<=))

import Streamly (IsStream, MonadAsync)


type SpiderConn n v e = (SpiderNodeId n, NodeAttributes v, LinkAttributes e)

type SpiderNodeId n = (ToJSON n)

newtype KbtzRoot n = KbtzRoot { getRoot :: n }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

mkKbtzRoot :: KbtzName -> KbtzRoot NodeMAC
mkKbtzRoot (KbtzId k) = (KbtzRoot (NodeId ("grid_" <> k) :: NodeMAC))

getGridRoot :: KbtzName -> NodeMAC
getGridRoot = getRoot . mkKbtzRoot

type GraphS t m n v e = (IsStream t, MonadAsync m, SpiderConn n v e) => t m ((n, v), t m (n -> e))

data KbtzConf = KbtzConf
  { thisKbtz :: KbtzName
  , mqttOpts :: MQTTOpts
  , spiderHost :: String
  , spiderPort :: Int
  } deriving (Eq, Ord, Show, Generic)




--getKibbutzim :: (KbtzM m) => m ([KbtzName])
--getKibbutzim = undefined

mkKbtzConf :: KbtzName -> MQTTOpts -> String -> Int -> KbtzConf
mkKbtzConf = KbtzConf


runKibbutz :: forall m. (KbtzM m NodeMAC, MonadCatch m) => KbtzConf -> m ()
runKibbutz KbtzConf{mqttOpts, thisKbtz, spiderHost, spiderPort} = do
  ns <- kbtzNodes thisKbtz
  lg <- liftIO $ newLogger Debug stdout
  qs' <-  (liftIO $ A.async (liftIO $ mqtt lg ns))
  MessageQs{stateChan, statsChan, outbox} <- liftIO $ A.wait qs'
  sensors <- sensorKbtz @AsyncT ns stateChan
  let txPlan = planTx horizon sensors
      txMonitor = (flip monitorTx sensors) <$> txPlan
      dispatcher = S.mapM (dispatchTx outbox) txPlan
      planHG = ingestThisKbtz stakeLinkDir $ (,)
               <$> stream sensors
               <*> (S.yield . (curryTx mempty) <$> txPlan)
      monHG = ingestThisKbtz txStatusLinkDir $ editMonS snd $ (,)
        <$> stream sensors
        <*> ((fmap . fmap) (curryTx (Source, mempty)) txMonitor)
  meshHG <- (pure . writeSpiderStream spConf addRTS . stream)
            =<< rsKbtz @AsyncT ns statsChan
  S.drain . adapt $
    dispatcher `parallel` monHG `parallel` planHG `parallel` meshHG
  where
    spConf :: forall v e. SpiderConn () v e => Config NodeMAC v e
    spConf = defConfig { wsHost = spiderHost, wsPort = spiderPort } 
    sensorKbtz :: forall t. (IsStream t, MonadAsync m)
      => [NodeMAC]
      -> WriteChan NodeMAC EnergyState
      -> m (Kbtz t m NodeMAC SensorS)
    sensorKbtz ns q = kbtz ns (sub @t @m @NodeMAC @EnergyState q) nodeS
    rsKbtz :: forall t. (IsStream t, MonadAsync m)
      => [NodeMAC]
      -> WriteChan NodeMAC RuntimeStats
      -> m (Kbtz t m NodeMAC RuntimeStats)
    rsKbtz ns q = kbtz ns (sub @t @m @NodeMAC @RuntimeStats q) id
    ingestThisKbtz :: forall t e. (IsStream t, LinkAttributes e)
      => (e -> LinkState) -> GraphS t m NodeMAC SensorS e -> t m ()
    ingestThisKbtz = ingestHyperGraph spConf (mkKbtzRoot thisKbtz) _time
    -- semantic editor combinator
    editMonS :: (Functor (t m)) => (c -> d) -> t m (a, t m (b -> c)) -> t m (a, t m (b -> d))
    editMonS = (fmap . second . fmap . result)
    mqtt lg ns = liftIO $
       withMqttAuth lg thisKbtz
         (runMqtt mqttOpts ns mkCallback)
    horizon = 60


ingestHyperGraph :: forall t m n v e.
  (KbtzConn t m n, MonadCatch m, SpiderConn n v e)
  => Config n v e
  -> KbtzRoot n -- $ NodeId Representing the Grid Root, serves as common edge for the hypergraph representation (Maybe?)
  -> (v -> Maybe UTCTime)   -- $ How to get a timestamp from the vertex
  -> (e -> LinkState)       -- $ How to get the edge direction
  -> GraphS t m n v e -- $ a stream of vertices and a stream of edges for each vertex
  -> t m ()
ingestHyperGraph conf (KbtzRoot gn) getTime getDir =
  writeSpiderStream conf (\s -> uncurry (addNodeWithEdges s))
  where
    addNodeWithEdges :: Spider n v e -> (n, v) -> t m (n -> e) -> m ()
    addNodeWithEdges spider nv edges = do
      t' <- liftIO now
      ls <- S.toList . adapt $ edges <*> (S.repeat $ fst nv)
      let
        t = fromMaybe t' $ fromUTCTime <$> (getTime . snd $ nv)
        lx = (toLink getDir) gn <$> ls
      liftIO $ addFoundNode spider $ toFN t nv lx

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
  (\s -> S.mapM (f s) as)

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
