{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes, ConstraintKinds, KindSignatures, QuantifiedConstraints, MultiParamTypeClasses, GADTs, FlexibleInstances#-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns  #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveAnyClass, DeriveFunctor, StandaloneDeriving, TupleSections, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chopaan.Kibbutz ( runKibbutz, runKibbutz', runKibbutzM
                       , KbtzC(..), KbtzScene, mkKbtzConf, S3Opts) where

import GHC.Generics ( Generic )

import Network.AWS.S3 (BucketName, ObjectKey(..))
import Chopaan.Types ( InfluxConn, MQTTOpts )

import ConCat.Misc (R)
import Control.Applicative ()
import Control.Arrow ( Arrow(second, first) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad ( (<=<) )
import Control.Monad.Catch ( MonadCatch )
import Control.Monad.STM ()
import Control.Monad.IO.Unlift ()
import Control.Monad.Bayes.Class ( MonadSample )
import Control.Concurrent.STM.TVar ()
import Control.Concurrent (forkIO)
import Control.Lens ()

import Data.Influxable
    ( lineMesh, lineFoldHttp, asKbtzNode, lineSensorR, wp )
import qualified Data.Map as M
import Data.Time ( getCurrentTime )
import Data.Text (pack)
import Data.Maybe ( isJust, fromJust, fromMaybe )
import Data.Bifunctor (bimap)
import Data.Greskell (runBinder)
import NetSpider.Graph (NodeAttributes(..))


import Streamly.Prelude as S (IsStream, MonadAsync, adapt)
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Fold.Tee as FL
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Pipe as P
import qualified Streamly.Data.Array.Foreign as A

import Proto.NodeMessageSchema.NodeMessages (RuntimeStats, EnergyState)
import qualified Proto.NodeMessageSchema.NodeMessages_Fields as N (cpuTime)
import System.IO (stdout)

import Chopaan.Kibbutz.KbtzId ( KbtzName )
import Chopaan.Kibbutz.Kibbutz ( KbtzConn )


import Chopaan.Kibbutz.Transactor ()
import Chopaan.Node.NodeId (NodeMAC)
import Chopaan.Node.Folds (SensorR, sensorFold)
import Chopaan.Node.Metrics (initSM)
import Chopaan.Node.Mesh (MeshNode, RxSignal, meshNodeLink)
import Chopaan.Node.HW (HW(..))
import Chopaan.Node.Components ()

import Chopaan.Comm.Mqtt.AWS (withMqttAuth)
import Chopaan.Kibbutz.AWS.Common (newLogger, LogLevel(..))
import Chopaan.Comm.Mqtt (runKibbutzGateway)
import Chopaan.Comm.Comm (MessageQs(..)
                         , Address(..)
                         , mkCallback
                         -- , mkCallback'
                         , PubQueue
                         , unfoldChan
                         , initMessageQs
                         )
import Chopaan.Utils.Streamly ()
import Chopaan.Graph.Kbtz ()
import Chopaan.Graph
    ( getGridRoot,
      addMeshNode,
      withSpider,
      runGraphWithDB,
      DBPools,
      GraphM )
import qualified Chopaan.Graph.Algebraic as AG
import Algebra.Graph.Label (Distance(..))

type S3Opts = BucketName

instance Eq (MessageQs n) where
  (==) = const (const False)

instance Ord (MessageQs n) where
  (<=) = const (const False)

instance Show (MessageQs n) where
  show = const "SomeQueue"

type KbtzG n = AG.Graph (Distance R) (n, HW R)

data KbtzC n = KbtzC
  { name :: KbtzName
  , structure :: KbtzG n
  , channelOpts :: Either MQTTOpts (MessageQs n)
  , s3Opts :: Maybe S3Opts
  , influxCon :: InfluxConn
  } deriving (Eq, Ord, Show, Generic)



kbtzNs :: (Ord n) => KbtzG n -> [n]
kbtzNs = fmap fst . AG.vertexList

kbtzHW :: (Ord n) => KbtzG n -> [(n, HW R)]
kbtzHW = AG.vertexList

nodeHWs :: (Ord n) => KbtzG n -> M.Map n (HW R)
nodeHWs = M.fromList . kbtzHW

newtype Kbtzim = Kbtzim { unKbtzim :: S.SerialT GraphM (Either KbtzName (KbtzName, NodeMAC)) }
  deriving (Generic)


mkKbtzConf :: KbtzName -> AG.Graph (Distance R) (n, HW R) -> Either MQTTOpts (MessageQs n) -> Maybe S3Opts -> InfluxConn -> KbtzC n
mkKbtzConf = KbtzC
{-# INLINE mkKbtzConf #-}

qSrc :: forall t m n. (KbtzConn t m n)
  => MessageQs n
  -> m (t m (n, EnergyState), t m (n, RuntimeStats), PubQueue)
qSrc (MessageQs{stateChan, statsChan, outbox}) = do
  sk <- unfoldChan stateChan
  rk <- unfoldChan statsChan
  return $ (sk, rk, outbox)
{-# INLINE qSrc #-}

mqttQs :: (MonadIO m) => (MessageQs NodeMAC) -> MQTTOpts -> KbtzName -> [NodeMAC] -> m ()
mqttQs qs opts name ns = do
  lg <- liftIO $ newLogger Debug stdout
  (liftIO $ withMqttAuth lg name
    (runKibbutzGateway name ns qs mkCallback opts))
{-# INLINE mqttQs #-}

-- mqttStreams :: (IsStream t, MonadAsync m, MonadUnliftIO m, Address n)
--   => MessageQs n
--   -> MQTTOpts
--   -> KbtzName
--   -> [n]
--   -> m (() -> m (), (t m (n, EnergyState), t m (n, RuntimeStats)))
-- mqttStreams qs opts name ns = do
--   (cb, (es, rs))<- mkCallback'
--   lg <- liftIO $ newLogger Info stdout
--   let c () = (liftIO $ withMqttAuth lg name
--               (runKibbutzGateway name ns qs (const cb) opts))
--   return (c, (es, rs))
-- {-# INLINE mqttStreams #-}

-- mqttSrc :: forall t m. (KbtzConn t m NodeMAC) => KbtzName -> [NodeMAC] -> MQTTOpts
--  -> m ((t m (NodeMAC, EnergyState), t m (NodeMAC, RuntimeStats), PubQueue))
-- mqttSrc k ns o = qSrc  =<< (mqttQs qs o k ns)



runKibbutz' :: forall t m. (IsStream t, MonadAsync m, MonadSample m, MonadCatch m)
  => DBPools -> KbtzC NodeMAC -> t m (KbtzScene NodeMAC)
runKibbutz' poo = S.concatM . (runKibbutzM poo)

runKibbutzM :: forall t m. (IsStream t, MonadAsync m, MonadSample m, MonadCatch m)
  => DBPools -> KbtzC NodeMAC -> m (t m (KbtzScene NodeMAC))
runKibbutzM poo = (pure . S.adapt . S.hoist (runGraphWithDB poo)) <=< (runGraphWithDB poo . runKibbutz)


type GridScene n = (M.Map n SensorR)--, Maybe (TxPlan n)), TxState n)

type MeshScene n = (n, (MeshNode, RxSignal))
type KbtzScene n = Either (GridScene n) (MeshScene n)
type GridEv = (SensorR) -- , Maybe Stake, Maybe TxStatus)

runKibbutz :: forall t. (IsStream t) => KbtzC NodeMAC -> GraphM (t GraphM (KbtzScene NodeMAC))
runKibbutz kc@KbtzC{name, structure, channelOpts, influxCon} = do
  -- Live Data
  liftIO . print $ show kc
  t0 <- liftIO $ getCurrentTime
  --gridFold <- withSpider $ saveTx name
  meshFold <- withSpider (addMeshNode @GraphM)
  let
    processES :: t GraphM (NodeMAC, EnergyState) -> t GraphM (GridScene NodeMAC)
    processES s = S.tapRate 60 (\x -> liftIO . print $ "Grid Processed Rate: " <> show x)
                $ S.map snd
                --   $ S.tap (FL.mapM (liftIO . print) (FL.lmap getLatest gridFold))
                --   $ status
                --   $ plan
                --  $ S.map (second Tx)
                $ S.tap (FL.lmap glS sLineF)
                $ gridSensorR structure
                $ S.tapRate 60 (\x -> liftIO . print $ "Grid Incoming Rate: " <> show x) s
    processRS s = S.tapRate 10 (\x -> liftIO . print $ "Mesh Processed Rate: " <> show x)
                $ S.tap mLineF -- meshFold)
                $ S.map (second (meshNodeLink $ getGridRoot name))
                $ S.tapRate 60 (\x -> liftIO . print $ "Mesh Incoming Rate: " <> show x) s
    liveStream es rs = (Left <$> (processES es))
                 `S.wAsync` (Right <$> (processRS rs))
  case channelOpts of
    (Left mqopts) -> do
      qs <- liftIO initMessageQs
      liftIO . forkIO $ mqttQs qs mqopts name nodes
      (es, rs, outbox) <- qSrc qs
      return $ liveStream es rs
    (Right qs) -> do
      (es, rs, outbox) <- qSrc qs
      return $ liveStream es rs
  where
    nodes = kbtzNs structure
    wp' = wp influxCon "chopaanMQTT" 
    glS :: (NodeMAC, (M.Map NodeMAC SensorR)) -> (NodeMAC, SensorR)
    glS (n, m) = (n, fromMaybe initSM (M.lookup n m))
    sLineF :: FL.Fold GraphM (NodeMAC, SensorR) ()
    sLineF = FL.lmap (\(n, x) -> lineSensorR (asKbtzNode name n) x) (lineFoldHttp 10 wp')
    mLineF :: FL.Fold GraphM (NodeMAC, (MeshNode, RxSignal)) ()
    mLineF = FL.lmap (\(n, x) -> lineMesh (asKbtzNode name n) x) (lineFoldHttp 10 wp')
      -- Stream Processors that run Folds
    -- getLatest ::
    --   (NodeMAC, ((NodeStates NodeMAC, Maybe (TxPlan NodeMAC)), (TxState NodeMAC)))
    --   -> (NodeMAC, (SensorR, Maybe Stake, Maybe TxStatus))
    -- getLatest (n, ((Tx a, b), c)) = let
    --   a' = fromMaybe initSM (M.lookup n a)
    --   b' = (M.lookup n . unTx) =<< b
    --   c' = snd <$> (M.lookup n (unTx c))
    --   in (n, (a', b', c'))
    -- {-# INLINE getLatest #-}
    horizon = 10 * 60
    {-# INLINE horizon #-}


gridSensorR :: (KbtzConn t m n) => KbtzG n -> t m (n, EnergyState) -> t m (n, M.Map n SensorR)
gridSensorR structure = S.map (first fromJust)
                        . S.filter (isJust . fst)
                        . S.postscan (FL.tee (FL.foldl' ((const (Just . fst))) Nothing)
                                       (FL.demux $ fmap sensorFold (nodeHWs structure)))
{-# INLINE gridSensorR #-}
