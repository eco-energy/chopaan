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
                                  , TransactionStatus
                                  , TxPlan
                                  , Stake
                                  , Tx(..)
                                  , Role(..)
                                  , curryTx)
import Chopaan.Kibbutz.Mesh

import Chopaan.Node.NodeId (NodeMAC)
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

spiderS :: forall t m n v e.
  (KbtzConn t m n, SpiderConn n v e)
  => (v -> Maybe UTCTime)
  -> Spider n v e
  -> t m ((n, v), t m (n -> (e, n)))
  -> m ()
spiderS getTimestamp s = S.drain . adapt . (fmap (uncurry addNode))
  where
    addNode :: (n, v) -> t m (n -> (e, n)) -> m ()
    addNode nv edges = do
      t <- liftIO now
      ls <- S.toList . adapt $ edges <*> (S.repeat $ fst nv)
      liftIO $ addFoundNode s $ toFN t nv ls
        where
          toFN :: Timestamp -> (n, v) -> [(e, n)] -> FoundNode n v e
          toFN t' (n, v) lx = FoundNode
            { subjectNode = n
            , foundAt = (fromMaybe t') $ (return . fromUTCTime) =<< getTimestamp v 
            , neighborLinks = (toLink <$> lx)
            , nodeAttributes = v
            }
            where
              toLink :: (e, n) -> FoundLink n e
              toLink (e, n') = FoundLink
                { targetNode = n'
                , linkState=LinkToSubject
                , linkAttributes = e
                }

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
  lg <- liftIO $ newLogger Debug stdout
  qs' <-  (liftIO $ A.async (liftIO $ mqtt lg ns))
  MessageQs{stateChan, statsChan, outbox} <- liftIO $ A.wait qs'
  sensors <- sensorKbtz @ParallelT ns stateChan
  runtime <- rsKbtz @ParallelT ns statsChan
  let txPlan = planTx horizon sensors
      txMonitor = (flip monitorTx sensors) <$> txPlan
      gridS = (,) <$> (stream sensors) <*> ((S.yield . (curryTx mempty)) <$> txPlan)
      monS = (,) <$> (stream sensors) <*> ((fmap . fmap) (curryTx (Source, mempty)) txMonitor)
  spiderS _time gridSpider gridS
  -- semantic editor combinator from http://conal.net/blog/posts/semantic-editor-combinators
  spiderS _time monitorSpider ((fmap . second . fmap . result) snd monS)
  rsStream meshSpider $ stream runtime
  return ()
  where
    mqtt lg nodes = liftIO $
       withMqttAuth lg name
         (runMqtt mqttOpts nodes mkCallback)
    horizon = 60
