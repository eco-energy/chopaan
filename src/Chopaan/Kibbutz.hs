{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes, ConstraintKinds, KindSignatures, QuantifiedConstraints, MultiParamTypeClasses, GADTs, FlexibleInstances#-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns, NoMonomorphismRestriction  #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveAnyClass, DeriveFunctor, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chopaan.Kibbutz where

-- (runKibbutz, mkKbtzConf, stakeConfig, meshConfig, statusConfig, getGridRoot)

import GHC.Generics

import Data.Aeson (ToJSON, FromJSON)
import Data.Greskell
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)

import Network.AWS.S3 (BucketName)
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


import Proto.NodeMessageSchema.NodeMessages (RuntimeStats, EnergyState)
import System.IO (stdout)

import NetSpider.Spider (Spider, addFoundNode, getSnapshot, getSnapshotSimple, connectWith, close)
import NetSpider.Spider.Config (Config(..), defConfig)
import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..))
import NetSpider.Found (FoundNode(..), FoundLink(..), LinkState(..))
import NetSpider.Timestamp (fromUTCTime, now, Timestamp)
import NetSpider.Snapshot (SnapshotGraph)
import NetSpider.Query (defQuery, Query(..), Extended(..), (<=..<=))

import Streamly.Prelude (IsStream, MonadAsync)


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
import Chopaan.Node.Metrics (SensorMetrics(..), Node(..))
import Chopaan.Node.HW

import Chopaan.Comm.Mqtt (runMqtt)
import Chopaan.Comm.S3
import Chopaan.Comm.Comm (MessageQs(..)
                         , mkCallback
                         , WriteChan
                         , PubQueue
                         , initPubQIO
                         )



type SpiderConn n v e = (SpiderNodeId n, NodeAttributes v, LinkAttributes e)

type SpiderNodeId n = (ToJSON n)

newtype KbtzRoot n = KbtzRoot { getRoot :: n }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

mkKbtzRoot :: KbtzName -> KbtzRoot NodeMAC
mkKbtzRoot (KbtzId k) = (KbtzRoot (NodeId ("grid_" <> k) :: NodeMAC))

getGridRoot :: KbtzName -> NodeMAC
getGridRoot = getRoot . mkKbtzRoot

type SensorGr t m n v e = (IsStream t, MonadAsync m, SpiderConn n v e) => t m ((n, v), t m (n -> e))


newtype Channel m n a = Channel {
  unChannel :: ChannelOpts -> (ParallelT m (n, a))
} deriving (Generic)




type S3Opts = BucketName
type ChannelOpts = Either MQTTOpts S3Opts



data KbtzC n = KbtzC
  { name :: KbtzName
  , nodes :: [n]
  , channelOpts :: ChannelOpts
  , spiderHost :: String
  , spiderPort :: Int
  } deriving (Generic)

-- ChannelOpts -> AsyncT m (NodeMAC, Node Watts, Node WattHours, (Watts, WattHours))

mkKbtzConf :: KbtzName -> [n] -> ChannelOpts -> String -> Int -> KbtzC n
mkKbtzConf = KbtzC


kbtzimFromQs :: forall t m. (KbtzConn t m NodeMAC)
  => [NodeMAC]
  -> MessageQs NodeMAC
  -> m (Kbtz t m NodeMAC SensorS, Kbtz t m NodeMAC RuntimeStats, PubQueue)
kbtzimFromQs ns (MessageQs{stateChan, statsChan, outbox}) = do
  sk <- sensorKbtzChan stateChan
  rk <- rsKbtzChan statsChan
  return ( sk
         , rk 
         , outbox )
  where
    sensorKbtzChan :: WriteChan NodeMAC EnergyState
      -> m (Kbtz t m NodeMAC SensorS)
    sensorKbtzChan q = kbtz ns (sub q) nodeS
    rsKbtzChan :: WriteChan NodeMAC RuntimeStats
      -> m (Kbtz t m NodeMAC RuntimeStats)
    rsKbtzChan q = kbtz ns (sub q) id


kbtzimFromS3 :: forall t m. (KbtzConn t m NodeMAC)
  => [NodeMAC]
  -> S3Opts
  -> m (Kbtz t m NodeMAC SensorS, Kbtz t m NodeMAC RuntimeStats, PubQueue)
kbtzimFromS3 ns bucket = do
  l <- liftIO $ newLogger Info stdout
  sk <- sensorKbtzS3 l
  rk <- rsKbtzS3 l
  outbox <- liftIO $ initPubQIO
  return ( sk
         , rk
         , outbox )
  where
    sensorKbtzS3 l = kbtz ns (\n -> pure $ (sensorS3 l bucket n)) nodeS
    rsKbtzS3 l = kbtz ns (\n -> pure $ (rsS3 l bucket n)) id


runKibbutz :: forall m. (MonadAsync m, MonadCatch m) => KbtzC NodeMAC -> m ()
runKibbutz KbtzC{name, nodes, channelOpts, spiderHost, spiderPort} = do
  lg <- liftIO $ newLogger Info stdout
  ((sensorKbtz :: Kbtz AheadT m NodeMAC SensorS)
   , (rsKbtz  :: Kbtz AheadT m NodeMAC RuntimeStats)
   , (outbox :: PubQueue)) <- case channelOpts of
    Left mqttOpts -> (kbtzimFromQs $ nodes)
      =<< (liftIO $
            (A.wait
              =<< A.async (liftIO $ withMqttAuth lg name
                            (runMqtt mqttOpts nodes mkCallback))))
    Right s3Opts -> kbtzimFromS3 nodes s3Opts
  let
      powerK = _powerT <$> sensorKbtz
      energyK = _energyT <$> sensorKbtz
      storage = _battery <$> sensorKbtz
      
      txPlan = planTx horizon sensorKbtz
      txMonitor = (flip monitorTx sensorKbtz) <$> txPlan
      dispatcher = S.mapM (dispatchTx outbox) txPlan
      planHG = ingestSensorKbtz stakeLinkDir $ (,)
               <$> stream sensorKbtz
               <*> (S.yield . (curryTx mempty) <$> txPlan)
      monHG = ingestSensorKbtz txStatusLinkDir $ editMonS snd $ (,)
        <$> stream sensorKbtz
        <*> ((fmap . fmap) (curryTx (Source, mempty)) txMonitor)
  meshHG <- pure . writeSpiderStream spConf addRTS . stream $ rsKbtz
   
  S.drain . adapt $
    monHG `parallel` planHG `parallel` meshHG `parallel` dispatcher
  where
    spConf :: forall v e. SpiderConn () v e => Config NodeMAC v e
    spConf = defConfig { wsHost = spiderHost, wsPort = spiderPort } 

    ingestSensorKbtz :: forall t e. (IsStream t, LinkAttributes e)
      => (e -> LinkState) -> SensorGr t m NodeMAC SensorS e -> t m ()
    ingestSensorKbtz = ingestHyperGraph spConf (mkKbtzRoot name) _time
    -- semantic editor combinator
    editMonS :: (Functor (t m)) => (c -> d) -> t m (a, t m (b -> c)) -> t m (a, t m (b -> d))
    editMonS = (fmap . second . fmap . result)
    horizon = 60



ingestHyperGraph :: forall t m n v e.
  (KbtzConn t m n, MonadCatch m, SpiderConn n v e)
  => Config n v e
  -> KbtzRoot n -- $ NodeId Representing the Grid Root, serves as common edge for the hypergraph representation (Maybe?)
  -> (v -> Maybe UTCTime)   -- $ How to get a timestamp from the vertex
  -> (e -> LinkState)       -- $ How to get the edge direction
  -> SensorGr t m n v e -- $ a stream of vertices and a stream of edges for each vertex
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
  (\s -> S.mapM (\x -> (liftIO . print $ "writing to spider") >> f s x) as)

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


