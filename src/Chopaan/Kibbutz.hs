{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes, ConstraintKinds, KindSignatures, QuantifiedConstraints, MultiParamTypeClasses, GADTs, FlexibleInstances#-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns  #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveAnyClass, DeriveFunctor, StandaloneDeriving, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chopaan.Kibbutz where

import GHC.Generics

import Data.Aeson (ToJSON, FromJSON)
import Data.Greskell
import Data.Hashable (Hashable)
import Data.Maybe
import Data.Time (UTCTime)
import Data.Either
import Data.Function
import Network.AWS.S3 (BucketName)
import Chopaan.Types hiding (DBOpts)
import Kbtz

import Control.DeepSeq (NFData)
import Control.Arrow (second, first, (***), (&&&))
import ConCat.Misc (result)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Concurrent (forkIO)
import qualified Data.Map as M
import qualified Control.Concurrent.Async as A

import Streamly.Prelude (IsStream, MonadAsync, adapt)
import Chopaan.Utils.Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as Internal
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Pipe as P
import Control.Monad

import Proto.NodeMessageSchema.NodeMessages (RuntimeStats, EnergyState)
import System.IO (stdout)

import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.Kibbutz

import Chopaan.Comm.Mqtt.AWS (withMqttAuth)
import Chopaan.Kibbutz.AWS.Common (newLogger, LogLevel(..))
import Chopaan.Kibbutz.Transactor
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
                         , initQs
                         , writeChan
                         )
import Chopaan.Graph.Greskell
import Chopaan.Graph
import Chopaan.Graph.Spider
import NetSpider.Spider.Config



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

  _ <- liftIO $ initGridRoot name
    
  let gridSensorS =
        S.postscan ((,)
                    <$> (FL.mkAccum_ ((const (Just . fst))) Nothing)
                    <*> sensorFD)
        es
  let tx = tapCount "statePipe"
           $ S.map (uncurry (&&))
           $ S.postscan (FL.lcatMaybes (saveTx (mkKbtzRoot name)))
           $ S.trace (liftIO . print . isNothing)
           $ S.map (fmap (\(n, se, st, (_, ts)) -> (n, (se, st, ts))))
           $ S.map unburden tx' -- S.zipAsyncWith unburden (tx') sTimer
        where
          tx' = (Internal.transform (tplOvrPipe) ((\(x, y) -> (x, Tx y)) <$> gridSensorS))
                      -- & S.trace (\(n, Just (x, y, z)) -> do
                      --       liftIO . print $ "NodeStates"
                      --       liftIO . print . M.keys . unTx $ x
                      --       liftIO . print $ "TxPlan"
                      --       liftIO . print . M.keys . unTx $ y
                      --       liftIO . print $ "TxState"
                      --       liftIO . print . M.keys . unTx $ z
                      --   ) 
          tplOvrPipe = P.zipWith (,) (P.map fst) pipeOvrTpl
          pipeOvrTpl = P.compose (statePipe horizon) (P.map snd)
        
  let meshS = tapCount "rsPipe" $ 
              S.postscan rsFD $ rs

  return . adapt $ meshS `S.parallel` tx
  where
    unburden :: (Ord n) => (Maybe n, Maybe (NodeStates n, TxPlan n, TxState n)) -> Maybe (n, SensorS, Stake, (Role, TransactionStatus))
    unburden (Nothing, _) = Nothing
    unburden (_, Nothing) = Nothing
    unburden ((Just n), Just (Tx sen, Tx pl, Tx st)) = do
      s <- (M.lookup n sen)
      p <- (M.lookup n pl)
      t <- (M.lookup n st)
      return $ (n, s, p, t)
    tapCount = S.tap . printCount
    printCount s = FL.mkAccumM_ (\x _ -> (liftIO . print $ s <> ": " <> (show x))
                                  >> (return $ x + (1 :: Int)))
                   (pure 0)
    --constBool = S.mapM (pure . (const True))
    sensorFD = FL.classify sensorFold
    rsFD = snd <$> ((,) <$> (FL.classify (meshFold)) <*> (addMeshNode))
    --processEither = FL.partition sensorFD rsFD
    dispatchTxSafe o t = tryJust t
      where
        tryJust (Just x) = expToBool
                         =<< (try $ (dispatchTx o x))
        tryJust Nothing = pure False
    horizon = 10 * 60
