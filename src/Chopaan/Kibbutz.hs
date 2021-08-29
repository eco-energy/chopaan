{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes, ConstraintKinds, KindSignatures, QuantifiedConstraints, MultiParamTypeClasses, GADTs, FlexibleInstances#-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns  #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveAnyClass, DeriveFunctor, StandaloneDeriving, TupleSections, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chopaan.Kibbutz ( runKibbutz, runKibbutz', runKibbutzM
                       , KbtzC(..), mkKbtzConf, S3Opts) where

import GHC.Generics

import Network.AWS.S3 (BucketName, ObjectKey(..))
import Chopaan.Types hiding (DBOpts)

import Control.Applicative
import Control.Arrow
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Catch
import Control.Monad.STM
import Control.Monad.IO.Unlift
import Control.Concurrent.STM.TVar
import Control.Concurrent (forkIO)
import Control.Lens

import qualified Data.Map as M
import Data.Time
import Data.Text (pack)
import Data.Maybe
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

import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.Kibbutz

import Chopaan.Comm.Mqtt.AWS (withMqttAuth)
import Chopaan.Kibbutz.AWS.Common (newLogger, LogLevel(..))
import Chopaan.Kibbutz.Transactor
import Chopaan.Node.NodeId (NodeMAC)
import Chopaan.Node.Folds (SensorR, sensorFold)
import Chopaan.Node.Metrics (initSM)
import Chopaan.Node.Mesh (MeshNode, RxSignal, meshNodeLink)



import Chopaan.Comm.Mqtt (runMqtt)
import Chopaan.Comm.Comm (MessageQs(..)
                         , Address(..)
                         , mkCallback
                         , mkCallback'
                         , PubQueue
                         , unfoldChan
                         , initMessageQs
                         )
import Chopaan.Utils.Streamly
import Chopaan.Graph.Kbtz
import Chopaan.Graph


type S3Opts = BucketName
type ChannelOpts = (MessageQs NodeMAC)

instance Eq (MessageQs n) where
  (==) = const (const False)

instance Ord (MessageQs n) where
  (<=) = const (const False)

instance Show (MessageQs n) where
  show = const "SomeQueue"

data KbtzC n = KbtzC
  { name :: KbtzName
  , nodes :: [n]
  , channelOpts :: Either MQTTOpts (MessageQs n)
  , s3Opts :: Maybe S3Opts
  } deriving (Eq, Ord, Show, Generic)


mkKbtzConf :: KbtzName -> [n] -> Either MQTTOpts (MessageQs n) -> Maybe S3Opts -> KbtzC n
mkKbtzConf = KbtzC

qSrc :: forall t m n. (KbtzConn t m n)
  => MessageQs n
  -> m (t m (n, EnergyState), t m (n, RuntimeStats), PubQueue)
qSrc (MessageQs{stateChan, statsChan, outbox}) = do
  sk <- unfoldChan stateChan
  rk <- unfoldChan statsChan
  return $ (sk, rk, outbox)


mqttQs :: (MonadIO m) => (MessageQs NodeMAC) -> MQTTOpts -> KbtzName -> [NodeMAC] -> m ()
mqttQs qs opts name ns = do
  lg <- liftIO $ newLogger Info stdout
  (liftIO $ withMqttAuth lg name
    (runMqtt name ns qs mkCallback opts))

mqttStreams :: (IsStream t, MonadAsync m, MonadUnliftIO m, Address n)
  => MessageQs n
  -> MQTTOpts
  -> KbtzName
  -> [n]
  -> m (() -> m (), (t m (n, EnergyState), t m (n, RuntimeStats)))
mqttStreams qs opts name ns = do
  (cb, (es, rs))<- mkCallback'
  lg <- liftIO $ newLogger Info stdout
  let c () = (liftIO $ withMqttAuth lg name
              (runMqtt name ns qs (const cb) opts))
  return (c, (es, rs))

-- mqttSrc :: forall t m. (KbtzConn t m NodeMAC) => KbtzName -> [NodeMAC] -> MQTTOpts
--  -> m ((t m (NodeMAC, EnergyState), t m (NodeMAC, RuntimeStats), PubQueue))
-- mqttSrc k ns o = qSrc  =<< (mqttQs qs o k ns)



runKibbutz' :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
  => DBPools -> KbtzC NodeMAC -> t m (KbtzScene NodeMAC)
runKibbutz' poo = S.concatM . (runKibbutzM poo)

runKibbutzM :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
  => DBPools -> KbtzC NodeMAC -> m (t m (KbtzScene NodeMAC))
runKibbutzM poo = (pure . S.adapt . S.hoist (runGraphWithDB poo)) <=< (runGraphWithDB poo . runKibbutz)


type GridScene n = ((NodeStates n, Maybe (TxPlan n)), TxState n)

type MeshScene n = (n, (MeshNode, RxSignal))
type KbtzScene n = Either (GridScene n) (MeshScene n)
type GridEv = (SensorR, Maybe Stake, Maybe TxStatus)

runKibbutz :: forall t. (IsStream t) => KbtzC NodeMAC -> GraphM (t GraphM (KbtzScene NodeMAC))
runKibbutz kc@KbtzC{name, nodes, channelOpts} = do
  -- Live Data
  liftIO . print $ show kc
  t0 <- liftIO $ getCurrentTime
  gridFold <- withSpider $ saveTx name
  meshFold <- withSpider addMeshNode
  case channelOpts of
    (Left mqopts) -> do
      qs <- liftIO initMessageQs
      liftIO . forkIO $ mqttQs qs mqopts name nodes
      (es, rs, outbox) <- qSrc qs
      return $ liveStream gridFold meshFold es rs
    (Right qs) -> do
      (es, rs, outbox) <- qSrc qs
      return $ liveStream gridFold meshFold es rs
  where
      -- Stream Processors that run Folds
    processES :: FL.Fold GraphM (NodeMAC, GridEv) Bool
              -> t GraphM (NodeMAC, EnergyState) -> t GraphM (GridScene NodeMAC)
    processES gridFold s = S.tapRate 30 (\x -> liftIO . print $ "Grid Processed Rate: " <> show x)
                S.|$ S.map snd
                S.|$ S.tap (FL.lmap getLatest gridFold)
                S.|$ status
                --S.|$ S.trace (dispatchTxSafe outbox . snd . snd)
                S.|$ plan
                S.|$ S.mapM (pure . second Tx)
                S.|$ gridSensorR nodes
                --  $ s
                S.|$ S.tapRate 30 (\x -> liftIO . print $ "Grid Incoming Rate: " <> show x) s
    processRS meshFold s = S.tapRate 30 (\x -> liftIO . print $ "Mesh Processed Rate: " <> show x)
                S.|$ S.tap meshFold
                S.|$ S.mapM (pure . second (meshNodeLink $ getGridRoot name))
                --  $ s
                S.|$ S.tapRate 30 (\x -> liftIO . print $ "Mesh Incoming Rate: " <> show x) s
    liveStream g m es rs = (Left <$> (processES g es))
                 `S.async` (Right <$> (processRS m rs))
    plan = S.postscan (secondF (dupF (transactionPlanner horizon)))
    status = S.postscan (secondF (txFold (Tx . M.fromList $ [(n, mempty @Stake) | n <- nodes])))
    getLatest ::
      (NodeMAC, ((NodeStates NodeMAC, Maybe (TxPlan NodeMAC)), (TxState NodeMAC)))
      -> (NodeMAC, (SensorR, Maybe Stake, Maybe TxStatus))
    getLatest (n, ((Tx a, b), c)) = let
      a' = fromMaybe initSM (M.lookup n a)
      b' = (\x -> M.lookup n (unTx x)) =<< b
      c' = snd <$> (M.lookup n (unTx c))
      in (n, (a', b', c'))
    {-# INLINE getLatest #-}
    -- dispatchTxSafe o t = tryJust' t
    --   where
    --     tryJust' (Just x) = expToBool
    --                      =<< (try $ (dispatchTx o x))
        -- tryJust' Nothing = pure False
    horizon = 10 * 60

gridSensorR :: (KbtzConn t m n) => [n] -> t m (n, EnergyState) -> t m (n, M.Map n SensorR)
gridSensorR ns s = S.map (first fromJust)
                   . S.filter (isJust . fst)
                   . S.postscan (FL.tee (FL.foldl' ((const (Just . fst))) Nothing)
                               sensorFD) $ s
  where
    sensorFD = FL.demux $ M.fromList $ (, sensorFold) <$> ns


-- bothUnfold :: forall m a b c d. (Monad m) => FL.Fold m a b -> FL.Fold m c d -> UF.Unfold m (a, c) (b, d)
-- bothUnfold f g = UF.zipWith (,) (foldUF fst f) (foldUF snd g)

-- foldUF :: forall m a b c. (Monad m) => (a -> c) -> FL.Fold m c b -> UF.Unfold m a b
-- foldUF fn fld = UF.functionM (UF.fold fld (UF.function fn))



-- pfart :: Monad m => FL.Fold m b x -> FL.Fold m c y -> FL.Fold m (Either b c) (Either x y)
-- pfart (FL.Fold stepL beginL doneL) (FL.Fold stepR beginR doneR) = FL.mkFold step begin done
--   where
--     begin = do
--       resL <- beginL
--       resR <- beginR
--       return $ resL 
    

