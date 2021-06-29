{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes, ConstraintKinds, KindSignatures, QuantifiedConstraints, MultiParamTypeClasses, GADTs, FlexibleInstances#-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns  #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveAnyClass, DeriveFunctor, StandaloneDeriving, TupleSections, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chopaan.Kibbutz where

import GHC.Generics

import Network.AWS.S3 (BucketName)
import Chopaan.Types hiding (DBOpts)

import Control.Applicative
import Control.Arrow
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Catch
import Control.Concurrent (forkIO)
import qualified Data.Map as M
import Data.Maybe
import qualified Control.Concurrent.Async as A

import Streamly as S
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as Internal
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Pipe as P

import Proto.NodeMessageSchema.NodeMessages (RuntimeStats, EnergyState)
import System.IO (stdout)

import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.Kibbutz

import Chopaan.Comm.Mqtt.AWS (withMqttAuth)
import Chopaan.Kibbutz.AWS.Common (newLogger, LogLevel(..))
import Chopaan.Kibbutz.Transactor
import Chopaan.Node.NodeId (NodeMAC)
import Chopaan.Node.Folds (SensorR, sensorFold, meshFold)
import Chopaan.Node.Metrics (initSM)


import Chopaan.Comm.Mqtt (runMqtt)
import Chopaan.Comm.S3
import Chopaan.Comm.Comm (MessageQs(..)
                         , mkCallback
                         , PubQueue
                         , unfoldChan
                         , initQs
                         , writeChan
                         , Address
                         )
import Chopaan.Graph


type S3Opts = BucketName
type ChannelOpts = Either (MessageQs NodeMAC) S3Opts

data KbtzC n = KbtzC
  { name :: KbtzName
  , nodes :: [n]
  , channelOpts :: ChannelOpts
  , spiderHost :: String
  , spiderPort :: Int
  } deriving (Generic)


mkKbtzConf :: KbtzName -> [n] -> ChannelOpts -> String -> Int -> KbtzC n
mkKbtzConf = KbtzC

qSrc :: forall t m n. (KbtzConn t m n)
  => MessageQs n
  -> m (t m (n, EnergyState), t m (n, RuntimeStats), PubQueue)
qSrc (MessageQs{stateChan, statsChan, outbox}) = do
  sk <- unfoldChan stateChan
  rk <- unfoldChan statsChan
  return $ (sk, rk, outbox)


twoSrc :: (KbtzConn t m n)
  => MessageQs n
  -> MessageQs n
  -> m (t m (n, EnergyState), t m (n, RuntimeStats), PubQueue)
twoSrc q q' = do
  (e, r, p) <- qSrc q
  (e', r', p') <- qSrc q'
  return $ (e `parallel` e', r `parallel` r', p')


s3Qs :: forall m. (MonadAsync m)
  => [NodeMAC]
  -> S3Opts
  -> m (MessageQs NodeMAC)
s3Qs ns bucket = do
  lg <- liftIO $ newLogger Info stdout
  qs <- liftIO $ initQs
  let x = (first fst) <$> (S.concatMapWith S.parallel (nodeS3 lg bucket) $ S.fromList ns)
  liftIO . forkIO $ S.mapM_ (\(n, x) -> case x of
              Left e -> liftIO $ writeChan (stateChan qs) n e
              Right r -> liftIO $ writeChan (statsChan qs) n r
          ) x
  return $ qs 
  

s3Src :: forall t m. (KbtzConn t m NodeMAC) => KbtzName -> [NodeMAC] -> S3Opts
  -> m ((t m (NodeMAC, EnergyState), t m (NodeMAC, RuntimeStats), PubQueue))
s3Src k ns opts = qSrc =<< (s3Qs ns opts)


mqttQs :: (MonadIO m) => MQTTOpts -> KbtzName -> [NodeMAC] -> m (MessageQs NodeMAC)
mqttQs opts name ns = do
  lg <- liftIO $ newLogger Info stdout
  liftIO $ (A.wait
              =<< A.async (liftIO $ withMqttAuth lg name
                            (runMqtt ns mkCallback opts)))

mqttSrc :: forall t m. (KbtzConn t m NodeMAC) => KbtzName -> [NodeMAC] -> MQTTOpts
  -> m ((t m (NodeMAC, EnergyState), t m (NodeMAC, RuntimeStats), PubQueue))
mqttSrc k ns o = qSrc  =<< (mqttQs o k ns)

-- propagateLastMaybe :: (IsStream t, MonadAsync m, Monoid a) => t m (Maybe a) -> t m a
-- propagateLastMaybe = S.postscan mf
--   where
--     mf = FL.mkFoldId lastOnNothingCurrentOnJust mempty
--     lastOnNothingCurrentOnJust a (Just a') = a'
--     lastOnNothingCurrentOnJust a Nothing = a


runKibbutz :: forall t m. (IsStream t, MonadAsync m, MonadCatch m, Monad (t m)) => KbtzC NodeMAC -> m (t m Bool)
runKibbutz KbtzC{name, nodes, channelOpts, spiderHost, spiderPort} = do
  (es, rs, outbox) <- case channelOpts of
    Left queues -> qSrc @t queues
    Right s3Opts -> qSrc =<< s3Qs nodes s3Opts

  spool <- mkSpool $ mkConfG (spiderHost, spiderPort)
  --_ <- liftIO $ runSpider spool $ initGridRoot name nodes
  gridFold <- liftIO $ runSpider spool (saveTx @m name)
  meshF <- runSpider spool (addMeshNode @m)

  let gridSensorR = S.postscan ((,)
                     <$> (FL.mkPureId ((const (Just . fst))) Nothing)
                     <*> sensorFD) es
      initPlan = Tx . M.fromList $ [(n, mempty @Stake) | n <- nodes]
      plan :: t m (NodeMAC, NodeStates NodeMAC)
        -> t m (NodeMAC, (NodeStates NodeMAC, Maybe (TxPlan NodeMAC)))
      plan = S.postscan (secondF (dupF (transactionPlanner horizon)))
  let tx = tapCount "statePipe"
           $ S.parallely . S.adapt
           $ S.postscan gridFold
           -- $ constBool
           -- $ S.trace (liftIO . print)
           $ S.map getLatest
           $ S.postscan (secondF (txFold initPlan))
           $ plan
           $ S.map (\(n, a) -> (fromJust n, a))
           $ S.filter (isJust . fst)
           $ fmap (\(x, y) -> (x, Tx y)) -- <$> gridSensorR)
           -- $ S.onException (liftIO . print $ "Exception Thrown")
           $ gridSensorR

  let meshS = tapCount "rsPipe" $ S.parallely . S.adapt $ S.postscan meshF rs -- constBool rs
  return $ (meshS `parallel` tx)
  where
    getLatest ::
      (NodeMAC, ((NodeStates NodeMAC, Maybe (TxPlan NodeMAC)), (TxState NodeMAC)))
      -> (NodeMAC, (SensorR, Maybe Stake, Maybe TxStatus))
    getLatest (n, ((Tx a, b), c)) = let
      a' = fromMaybe initSM (M.lookup n a)
      b' = (\x -> M.lookup n (unTx x)) =<< b
      c' = snd <$> (M.lookup n (unTx c))
      in (n, (a', b', c'))
      where
        getN :: M.Map NodeMAC a -> a
        getN = flip (M.!) n
    tapCount = S.tap . printCount
    printCount s = FL.mkFoldId (\x _ -> (liftIO . print $ s <> ": " <> (show x))
                                  >> (return $ x + (1 :: Int)))
                   (pure 0)
    constBool :: t m a -> t m Bool
    constBool = S.map (const True)
    sensorFD = FL.demux $ M.fromList $ (, sensorFold) <$> nodes
    rsFD saveMF = saveMF
    --processEither = FL.partition sensorFD rsFD
    --tryToBool = expToBool <=< try
    dispatchTxSafe o t = tryJust t
      where
        tryJust (Just x) = expToBool
                         =<< (try $ (dispatchTx o x))
        tryJust Nothing = pure False
    horizon = 10 * 60



