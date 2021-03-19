{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes, ConstraintKinds #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses, GADTs, FlexibleInstances #-}
module Chopaan.Kibbutz.Kibbutzim where

import GHC.Generics
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)
import Data.Aeson (ToJSON)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)

import Chopaan.Types
import Kbtz

import Control.Arrow (first, second)
import ConCat.Misc (result)
import Control.Monad.IO.Class
import qualified Control.Concurrent.Async as A

import Streamly
import qualified Streamly.Prelude as S

import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.Kibbutz

import Chopaan.Kibbutz.AWS.Things (withMqttAuth)
import Chopaan.Kibbutz.AWS.Common (newLogger, LogLevel(..))
import Chopaan.Kibbutz.Transactor (runTransactor
                                  , planTx
                                  , monitorTx
                                  , dispatchTx
                                  , TransactionStatus
                                  , TxPlan
                                  , Stake
                                  , Tx(..)
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
import Proto.NodeMessageSchema.NodeMessages (RuntimeStats, HardwareConfig, EnergyState)
import Chopaan.Comm.Mqtt (runMqtt)
import Chopaan.Comm.Comm (MessageQs(..)
                         , mkCallback
                         , WriteChan
                         )
import System.IO (stdout)
import NetSpider.Spider
  (Spider, addFoundNode)

import NetSpider.Graph (NodeAttributes, LinkAttributes)
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Timestamp (fromUTCTime, now, Timestamp)
import NetSpider.Snapshot (nodeId, nodeTimestamp, linkNodePair, linkTimestamp)

import Streamly
import qualified Streamly.Prelude as S

data Spiders n = Spiders
  { gridSpider :: GridSpider n
  , monitorSpider :: MonitorSpider n
  , meshSpider :: MeshSpider n
  }


type GridSpider n = Spider n SensorS Stake

type MonitorSpider n = Spider n SensorS TransactionStatus

type MeshSpider n = Spider n MeshNode RxSignal

type SpiderConn n v e = (ToJSON n, NodeAttributes v, LinkAttributes e)


sensorKbtz :: forall t m. (IsStream t, MonadAsync m)
  => [NodeMAC]
  -> WriteChan NodeMAC EnergyState
  -> m (Kbtz t m NodeMAC SensorS)
sensorKbtz ns q = kbtz ns (sub @t @m @NodeMAC @EnergyState q) nodeS

rsKbtz :: forall t m. (IsStream t, MonadAsync m)
  => [NodeMAC]
  -> WriteChan NodeMAC RuntimeStats
  -> m (Kbtz t m NodeMAC RuntimeStats)
rsKbtz ns q = kbtz ns (sub @t @m @NodeMAC @RuntimeStats q) id


data KbtzOpts = KbtzOpts { txHorizon :: Int }


runKibbutz :: forall m. (KbtzM m NodeMAC) => Spiders NodeMAC -> MQTTOpts -> KbtzName -> m ()
runKibbutz Spiders{..} mqttOpts name = do
  ns <- kbtzNodes name
  let gridNode = (NodeId ("grid_" <> (unKbtzId name)) :: NodeMAC)
  lg <- liftIO $ newLogger Debug stdout
  qs' <-  (liftIO $ A.async (liftIO $ mqtt lg ns))
  MessageQs{stateChan, statsChan, outbox} <- liftIO $ A.wait qs'
  sensors <- sensorKbtz @ParallelT ns stateChan
  runtime <- rsKbtz @ParallelT ns statsChan
  let txPlan = S.trace (dispatchTx outbox) $ planTx horizon sensors
      txMonitor = (flip monitorTx sensors) <$> txPlan
      gridS = (,) <$> (stream sensors) <*> ((S.yield . (curryTx mempty)) <$> txPlan)
      monS = (,) <$> (stream sensors) <*> ((fmap . fmap) (curryTx (Source, mempty)) txMonitor)
  addGrid gridSpider gridNode _time stakeLinkDir gridS
  -- semantic editor combinator from http://conal.net/blog/posts/semantic-editor-combinators
  addGrid monitorSpider gridNode _time txStatusLinkDir ((fmap . second . fmap . result) snd monS)
  rsStream meshSpider $ stream runtime
  return ()
  where
    mqtt lg ns = liftIO $
       withMqttAuth lg name
         (runMqtt mqttOpts ns mkCallback)
    horizon = 60

addGrid :: forall t m n v e.
  (KbtzConn t m n, SpiderConn n v e)
  => Spider n v e
  -> n                      -- $ Node Representing the Grid
  -> (v -> Maybe UTCTime)   -- $ How to get a timestamp from the vertex
  -> (e -> LinkState)       -- $ How to get the edge direction
  -> t m ((n, v), t m (n -> e)) -- $ a stream of nodes and a stream of edges for each node
  -> m ()
addGrid s gridNode getTimestamp getDirection = S.drain . adapt . (fmap (uncurry addNode))
  where
    addNode :: (n, v) -> t m (n -> e) -> m ()
    addNode nv edges = do
      t <- liftIO now
      ls <- S.toList . adapt $ edges <*> (S.repeat $ fst nv)
      liftIO $ addFoundNode s $ toFN t nv ls
        where
          toFN :: Timestamp -> (n, v) -> [e] -> FoundNode n v e
          toFN t' (n, v) lx = FoundNode
            { subjectNode = n
            , foundAt = (fromMaybe t') $ (return . fromUTCTime) =<< getTimestamp v 
            , neighborLinks = ((toLink gridNode) <$> lx)
            , nodeAttributes = v
            }
            where
              toLink :: n -> e -> FoundLink n e
              toLink n' e = FoundLink
                { targetNode = n'
                , linkState = (getDirection e)
                , linkAttributes = e
                }
