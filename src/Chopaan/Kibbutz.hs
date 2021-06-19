{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes, ConstraintKinds, KindSignatures, QuantifiedConstraints, MultiParamTypeClasses, GADTs, FlexibleInstances#-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns  #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveAnyClass, DeriveFunctor, StandaloneDeriving, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chopaan.Kibbutz where

import GHC.Generics

import Network.AWS.S3 (BucketName)
import Chopaan.Types hiding (DBOpts)


import Control.Arrow
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Concurrent (forkIO)
import qualified Data.Map as M
import Data.Maybe
import qualified Control.Concurrent.Async as A

import Streamly.Prelude (IsStream, MonadAsync, adapt)
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as Internal
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



import Chopaan.Comm.Mqtt (runMqtt)
import Chopaan.Comm.S3
import Chopaan.Comm.Comm (MessageQs(..)
                         , mkCallback
                         , PubQueue
                         , unfoldChan
                         , initQs
                         , writeChan
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

qSrc :: forall t m. (KbtzConn t m NodeMAC)
  => MessageQs NodeMAC
  -> m (t m (NodeMAC, EnergyState), t m (NodeMAC, RuntimeStats), PubQueue)
qSrc (MessageQs{stateChan, statsChan, outbox}) = do
  sk <- unfoldChan stateChan
  rk <- unfoldChan statsChan
  return $ (sk, rk, outbox)


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
  

mqttQs :: (MonadIO m) => MQTTOpts -> KbtzName -> [NodeMAC] -> m (MessageQs NodeMAC)
mqttQs opts name ns = do
  lg <- liftIO $ newLogger Info stdout
  liftIO $ (A.wait
              =<< A.async (liftIO $ withMqttAuth lg name
                            (runMqtt opts ns mkCallback)))

mqttSrc :: forall t m. (KbtzConn t m NodeMAC) => KbtzName -> [NodeMAC] -> MQTTOpts
  -> m ((t m (NodeMAC, EnergyState), t m (NodeMAC, RuntimeStats), PubQueue))
mqttSrc k ns o = qSrc  =<< (mqttQs o k ns)


runKibbutz :: forall t m. (IsStream t, MonadAsync m, MonadCatch m, Monad (t m)) => KbtzC NodeMAC -> m (t m Bool)
runKibbutz KbtzC{name, nodes, channelOpts, spiderHost, spiderPort} = do
  (es, rs, outbox) <- case channelOpts of
    Left queues -> qSrc @t queues
    Right s3Opts -> qSrc =<< s3Qs nodes s3Opts

  spool <- mkSpool $ mkConfG (spiderHost, spiderPort)
  _ <- liftIO $ runSpider spool $ initGridRoot name nodes

  gridFold <- liftIO $ runSpider spool (saveTx name)
  let gridSensorR =
        S.postscan ((,)
                    <$> (FL.mkAccum_ ((const (Just . fst))) Nothing)
                    <*> sensorFD)
        es
      plan :: t m (NodeMAC, NodeStates NodeMAC)
        -> t m (NodeMAC, (NodeStates NodeMAC, Maybe (TxPlan NodeMAC)))
      plan = S.postscan (secondF (dupF (transactionPlanner horizon)))
  let tx = tapCount "statePipe"
           $ S.map (uncurry (&&))
           $ S.postscan gridFold
           $ S.map (\(n, ((a, b), c)) -> (n, a, b, c))
           $ S.postscan (secondF txFold)
           $ plan
           $ S.map (\(n, a) -> (fromJust n, a))
           $ S.filter (isJust . fst)
           $ ((\(x, y) -> (x, Tx y)) <$> gridSensorR)

  meshSF <- runSpider spool addMeshNode
  let meshS = tapCount "rsPipe" $ S.postscan (rsFD meshSF) rs

  return $ (meshS) `S.parallel` (adapt tx)
  where
    tapCount = S.tap . printCount
    printCount s = FL.mkAccumM_ (\x _ -> (liftIO . print $ s <> ": " <> (show x))
                                  >> (return $ x + (1 :: Int)))
                   (pure 0)
    --constBool = S.mapM (pure . (const True))
    sensorFD = FL.classify sensorFold
    rsFD saveMF = snd <$> ((,) <$> (FL.classify meshFold) <*> saveMF)
    --processEither = FL.partition sensorFD rsFD
    dispatchTxSafe o t = tryJust t
      where
        tryJust (Just x) = expToBool
                         =<< (try $ (dispatchTx o x))
        tryJust Nothing = pure False
    horizon = 10 * 60
