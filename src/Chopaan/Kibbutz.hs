{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes, ConstraintKinds, KindSignatures, QuantifiedConstraints, MultiParamTypeClasses, GADTs, FlexibleInstances#-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns, NoMonomorphismRestriction  #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveAnyClass, DeriveFunctor, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chopaan.Kibbutz where

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

import qualified Data.Map as M
import qualified Control.Concurrent.Async as A

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL

import Proto.NodeMessageSchema.NodeMessages (RuntimeStats, EnergyState)
import System.IO (stdout)


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

import Chopaan.Node.NodeId (NodeId(..), NodeMAC)
import Chopaan.Node.Folds (SensorS, sensorFold, energyFold, demandFold, powerFold, timeFold, meshFold)
import Chopaan.Node.Node (nodeS)
import Chopaan.Node.Metrics (SensorMetrics(..), Node(..))
import Chopaan.Node.HW

import Chopaan.Node.Mesh

import Chopaan.Comm.Mqtt (runMqtt)
import Chopaan.Comm.S3
import Chopaan.Comm.Comm (MessageQs(..)
                         , mkCallback
                         , WriteChan
                         , PubQueue
                         , initPubQIO
                         , unfoldChan
                         )
import Chopaan.Graph.Greskell
import Chopaan.Graph
import Chopaan.Graph.Spider
import NetSpider.Spider.Config


newtype Channel m n a = Channel {
  unChannel :: ChannelOpts -> (ParallelT m (n, a))
} deriving (Generic)


type S3Opts = BucketName
type ChannelOpts = Either (MessageQs NodeMAC) S3Opts



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


sensorFD = FL.classify sensorFold
rsFD = FL.classify meshFold

qKbtz :: forall t m. (KbtzConn t m NodeMAC)
  => [NodeMAC]
  -> MessageQs NodeMAC
  -> m (Kbtz t m NodeMAC SensorS, Kbtz t m NodeMAC (MeshNode, RxSignal), PubQueue)
qKbtz ns (MessageQs{stateChan, statsChan, outbox}) = do
  sk <- unfoldChan stateChan
  rk <- unfoldChan statsChan
  return ( Kbtz (S.scan sensorFD sk)
         , Kbtz (S.scan rsFD rk)
         , outbox )


s3Kbtz :: forall t m. (KbtzConn t m NodeMAC)
  => [NodeMAC]
  -> S3Opts
  -> m (Kbtz t m NodeMAC SensorS, Kbtz t m NodeMAC (MeshNode, RxSignal), PubQueue)
s3Kbtz ns bucket = do
  l <- liftIO $ newLogger Info stdout
  outbox <- liftIO $ initPubQIO
  return ( sk undefined
         , rk undefined
         , outbox )
  where
    sk = Kbtz . (S.scan sensorFD)
    rk = Kbtz . (S.scan rsFD)
    -- alls l = nodeS3 l bucket n

mqttQs :: (MonadIO m) => MQTTOpts -> KbtzName -> [NodeMAC] -> m (MessageQs NodeMAC)
mqttQs opts name ns = do
  lg <- liftIO $ newLogger Info stdout
  liftIO $ (A.wait
              =<< A.async (liftIO $ withMqttAuth lg name
                            (runMqtt opts ns mkCallback)))

mqttKbtz :: forall t m. (KbtzConn t m NodeMAC) => KbtzName -> [NodeMAC] -> MQTTOpts
  -> m (Kbtz t m NodeMAC SensorS, Kbtz t m NodeMAC (MeshNode, RxSignal), PubQueue)  
mqttKbtz k ns o = qKbtz ns  =<< (mqttQs o k ns)


runKibbutz :: forall t m. (IsStream t, MonadAsync m, MonadCatch m, Monad (t m)) => KbtzC NodeMAC -> m (t m Bool)
runKibbutz KbtzC{name, nodes, channelOpts, spiderHost, spiderPort} = do
  ((sensorKbtz :: Kbtz t m NodeMAC SensorS)
   , (rsKbtz  :: Kbtz t m NodeMAC (MeshNode, RxSignal))
   , (outbox :: PubQueue)) <- case channelOpts of
    Left queues -> qKbtz nodes queues
    Right s3Opts -> s3Kbtz nodes s3Opts
  let
    counts = FL.classify FL.sum
    --powerK = _powerT <$> sensorKbtz
    --energyK = _energyT <$> sensorKbtz
    --storage = _battery <$> sensorKbtz
      
    --txPlan = S.map (const True) $ stream sensorKbtz -- planTx horizon
    --txMonitor = (flip monitorTx sensorKbtz) <$> txPlan
    --dispatcher = S.trace (liftIO . print) $ S.mapM (dispatchTxSafe outbox) txPlan

    --planHG = ingestSensorKbtz $ (,)
    --         <$> (S.trace (liftIO . print) $ stream sensorKbtz)
    --         <*> (S.yield . (curryTx mempty) <$> txPlan)
    -- monHG = ingestSensorKbtz $ editMonS snd $ (,)
    --         <$> stream sensorKbtz
    --         <*> ((fmap . fmap) (curryTx (Source, mempty)) txMonitor)
  --return $ -- S.map (\_ -> True)
  let xs = S.map (const True) $ stream rsKbtz
  let ys = S.map (const True) $ stream sensorKbtz
  --meshHG <- pure . writeSpiderStream spConf addRTSSafe $ xs
   
  return . adapt $ xs --`async` txPlan
    {--monHG `parallel`  planHG `parallel` `parallel` dispatcher meshHG `parallel` meshHG --}
  where
    addRTSSafe a b = expToBool =<< (try $ addRTS a b)
    dispatchTxSafe o t = expToBool =<< (try $ dispatchTx o t)
    spConf :: forall v e. SpiderConn () v e => Config NodeMAC v e
    spConf = defConfig { wsHost = spiderHost, wsPort = spiderPort } 

    ingestSensorKbtz :: forall t e. (IsStream t, LinkAttributes e, HasDir e)
      =>  GrS t m NodeMAC SensorS e -> t m Bool
    ingestSensorKbtz = ingestHyperGraph spConf (mkKbtzRoot name)
    -- semantic editor combinator
    editMonS :: (Functor (t m)) => (c -> d) -> t m (a, t m (b -> c)) -> t m (a, t m (b -> d))
    editMonS = (fmap . second . fmap . result)
    horizon = 60






