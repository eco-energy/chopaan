{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes, ConstraintKinds, KindSignatures, QuantifiedConstraints, MultiParamTypeClasses, GADTs, FlexibleInstances#-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns  #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveAnyClass, DeriveFunctor, StandaloneDeriving, TupleSections, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chopaan.Kibbutz where

import GHC.Generics

import Network.AWS.S3 (BucketName, ObjectKey(..))
import Chopaan.Types hiding (DBOpts)

import Control.Arrow
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Catch
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import qualified Data.Map as M
import Data.Maybe
import Data.Bifunctor (bimap)
import qualified Control.Concurrent.Async as A

import Streamly as S
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold.Types as FL
import qualified Streamly.Internal.Data.Unfold as UF

import Proto.NodeMessageSchema.NodeMessages (RuntimeStats, EnergyState)
import System.IO (stdout)

import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz.Kibbutz

import Chopaan.Comm.Mqtt.AWS (withMqttAuth)
import Chopaan.Kibbutz.AWS.Common (newLogger, LogLevel(..))
import Chopaan.Kibbutz.Transactor
import Chopaan.Node.NodeId (NodeMAC)
import Chopaan.Node.Folds (SensorR, sensorFold)
import Chopaan.Node.Metrics (initSM)
import Chopaan.Node.Mesh (MeshNode, RxSignal, meshNodeLink, meshF)

import Chopaan.Comm.Mqtt (runMqtt)
import Chopaan.Comm.S3
import Chopaan.Comm.Comm (MessageQs(..)
                         , mkCallback
                         , PubQueue
                         , unfoldChan
                         )
import Chopaan.Graph.Kbtz
import Chopaan.Graph
import Data.Time

type S3Opts = BucketName
type ChannelOpts = (MessageQs NodeMAC)

data KbtzC n = KbtzC
  { name :: KbtzName
  , nodes :: [n]
  , channelOpts :: ChannelOpts
  , s3Opts :: Maybe S3Opts
  } deriving (Generic)


mkKbtzConf :: KbtzName -> [n] -> ChannelOpts -> Maybe S3Opts -> KbtzC n
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
  (e, r, _) <- qSrc q
  (e', r', p') <- qSrc q'
  return $ (e `parallel` e', r `parallel` r', p')


type S3S (t :: (* -> *) -> * -> *) m a b = t m ((NodeMAC, ObjectKey, Maybe UTCTime), Either (NodeMAC, a) (NodeMAC, b))


s3Stream :: (IsStream t, MonadAsync m, MonadCatch m)
         => [(NodeMAC, Maybe ObjectKey)]
         -> S3Opts
         -> S3S t m EnergyState RuntimeStats
s3Stream ns bucket = let
  s' = S.bracketIO (liftIO $ newLogger Info stdout) (pure) s
  align :: (a, ((b, c), d)) -> ((b, a, c), (b, d))
  align (a, ((b, c), d)) = ((b, a, c), (b, d)) 
  f :: (n, Either a b) -> Either (n, a) (n, b)
  f (n, c) = case c of
    Left x -> Left (n, x)
    Right y -> Right (n, y)
  in (fmap (second f) $ fmap align s')
  where
    s lg = S.concatMapWith S.parallel (uncurry (nodeS3 lg bucket)) $ S.fromList ns


mqttQs :: (MonadIO m) => MQTTOpts -> KbtzName -> [NodeMAC] -> m (MessageQs NodeMAC)
mqttQs opts name ns = do
  lg <- liftIO $ newLogger Info stdout
  liftIO $ (A.wait
              =<< A.async (liftIO $ withMqttAuth lg name
                            (runMqtt ns mkCallback opts)))

mqttSrc :: forall t m. (KbtzConn t m NodeMAC) => KbtzName -> [NodeMAC] -> MQTTOpts
  -> m ((t m (NodeMAC, EnergyState), t m (NodeMAC, RuntimeStats), PubQueue))
mqttSrc k ns o = qSrc  =<< (mqttQs o k ns)



runKibbutz' :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
  => String -> Int -> KbtzC NodeMAC -> t m (KbtzScene NodeMAC)
runKibbutz' h p = S.concatM . (runKibbutzM h p)

runKibbutzM :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
  => String -> Int -> KbtzC NodeMAC -> m (t m (KbtzScene NodeMAC))
runKibbutzM h p = (pure . S.adapt . S.hoist (runGraphM h p)) <=< (runGraphM h p . runKibbutz)


type GridScene n = ((NodeStates n, Maybe (TxPlan n)), TxState n)

type MeshScene n = (n, (MeshNode, RxSignal))
type KbtzScene n = Either (GridScene n) (MeshScene n)


runKibbutz :: forall t. (IsStream t) => KbtzC NodeMAC -> GraphM (t GraphM (KbtzScene NodeMAC))
runKibbutz KbtzC{name, nodes, channelOpts} = do
  -- Live Data
  (es, rs, outbox) <- qSrc @t channelOpts
  -- Folds
  gridFold <- withSpider $ saveTx name
  meshFold <- withSpider addMeshNode
  -- Stream Processors that run Folds
  let
    processES :: t GraphM (NodeMAC, EnergyState) -> t GraphM (GridScene NodeMAC)
    processES = tapCount "esPipe"
                . S.map snd
                . S.tap (FL.lmap getLatest gridFold)
                . status
                -- . S.trace (dispatchTxSafe outbox . snd . snd)
                . plan
                . (fmap (second Tx))
                . gridSensorR nodes

    processRS = tapCount "rsPipe" . S.tap meshFold . S.map (second meshNodeLink)
  let
    liveStream = (Left <$> (processES es)) `parallel` (Right <$> (processRS rs)) 
  return liveStream
  where
    plan = S.postscan (secondF (dupF (transactionPlanner horizon)))
    {-# INLINE plan #-}
    status = S.postscan (secondF (txFold (Tx . M.fromList $ [(n, mempty @Stake) | n <- nodes])))
    {-# INLINE status#-}
    getLatest ::
      (NodeMAC, ((NodeStates NodeMAC, Maybe (TxPlan NodeMAC)), (TxState NodeMAC)))
      -> (NodeMAC, (SensorR, Maybe Stake, Maybe TxStatus))
    getLatest (n, ((Tx a, b), c)) = let
      a' = fromMaybe initSM (M.lookup n a)
      b' = (\x -> M.lookup n (unTx x)) =<< b
      c' = snd <$> (M.lookup n (unTx c))
      in (n, (a', b', c'))
    {-# INLINE getLatest #-}
    tapCount :: forall m a. (MonadAsync m, Show a) => String -> t m a -> t m a
    tapCount = S.tap . printCount
    printCount s = FL.mkFoldId (\x a ->
                                  (liftIO . print $ s <> ": " <> (show x))
                                  >> (return $ x + (1 :: Int)))
                   (pure 0)
    dispatchTxSafe o t = tryJust' t
      where
        tryJust' (Just x) = expToBool
                         =<< (try $ (dispatchTx o x))
        tryJust' Nothing = pure False
    horizon = 10 * 60

gridSensorR :: (KbtzConn t m n) => [n] -> t m (n, EnergyState) -> t m (n, M.Map n SensorR)
gridSensorR ns s = S.map (first fromJust)
                   . S.filter (isJust . fst)
                   . S.postscan ((,)
                               <$> (FL.mkPureId ((const (Just . fst))) Nothing)
                               <*> sensorFD) $ s
  where
    sensorFD = FL.demux $ M.fromList $ (, sensorFold) <$> ns
    {-# INLINE sensorFD #-}
{-# INLINE gridSensorR #-}

hydrateKbtz' :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
  => String -> Int -> KbtzC NodeMAC -> t m Hydration
hydrateKbtz' h p = S.concatM . (hydrateKbtzM h p)

hydrateKbtzM :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
  => String -> Int -> KbtzC NodeMAC -> m (t m Hydration)
hydrateKbtzM h p = (pure . S.adapt . S.hoist (runGraphM h p)) <=< (runGraphM h p . hydrateKbtz)

type Hydration = ((NodeMAC, ObjectKey, Maybe UTCTime)
                 , ((Maybe NodeMAC, M.Map NodeMAC SensorR)
                   , (Maybe NodeMAC, M.Map NodeMAC (MeshNode, RxSignal))))

type Hydration' = ((NodeMAC, ObjectKey, Maybe UTCTime)
                 , ((NodeMAC, M.Map NodeMAC SensorR)
                   , (NodeMAC, M.Map NodeMAC (MeshNode, RxSignal))))

hydrateKbtz :: forall t. (IsStream t) => KbtzC NodeMAC -> GraphM (t GraphM (Hydration))
hydrateKbtz KbtzC{name, nodes, s3Opts} = case s3Opts of
      Nothing -> return $ S.nil
      Just s -> do
        --x <- liftIO $ newTVarIO (M.fromList [])
        -- lsyncs <- mapM (\n -> do
        --                    ls <- withKbtzPool (flip getNodeLastSync n) 
        --                    return $ (\a -> (n, ObjectKey <$> a)) (listToMaybe ls)
        --                ) nodes
        -- flowFoldS3 <- (FL.lmap (getLatest initSM)) <$> (withSpider $ addFlowNode name)
        -- meshFoldS3 <- (FL.lmap (getLatest undefined)) <$> (withSpider addMeshNode)
        let
          lsyncs = zip nodes (repeat Nothing)
          srcs :: t GraphM ((NodeMAC, ObjectKey, Maybe UTCTime), Either (NodeMAC, EnergyState) (NodeMAC, RuntimeStats))
          srcs = s3Stream lsyncs s -- S.trace (thatF . fst) $ 
            --where
              --thatF :: (NodeMAC, ObjectKey, Maybe UTCTime) -> GraphM ()
              --thatF (n, k, t) = liftIO . atomically $ modifyTVar' x (M.insert n (k, t))
          
          -- saveUF :: UF.Unfold GraphM ((NodeMAC, M.Map NodeMAC SensorR), (NodeMAC, M.Map NodeMAC (MeshNode, RxSignal))) (Bool, Bool)
          -- saveUF = bothUnfold flowFoldS3 meshFoldS3

          -- saveAndId = UF.map snd $ UF.teeZipWith (,) saveUF UF.identity

          -- saveAndIdWithKey :: UF.Unfold GraphM Hydration Hydration
          -- saveAndIdWithKey = UF.teeZipWith (,) (UF.singleton fst) (UF.discardFirst saveAndId)
          
          process :: UF.Unfold GraphM ((NodeMAC, ObjectKey, Maybe UTCTime), Either (NodeMAC, EnergyState) (NodeMAC, RuntimeStats)) Hydration
          process = UF.teeZipWith (,) (UF.singleton fst) (foldUF snd (eitherWalay nodes))

        -- UF.map (bimap (first fromJust) (first fromJust)) $
        --                                                   (UF.filter (\((a, _), (b, _)) -> ((isJust $ a) && (isJust $ b)))) $
                                                          
        -- let finalize = do
        --       m <- liftIO . atomically $ readTVar x
        --       mapM_ (\(n, ((ObjectKey k), _)) ->
        --                                withKbtzPool (\p -> addLastSyncToHH p n k)) $ M.toList m
        --let f = UF.concat process saveAndIdWithKey
        return $ S.trace (\_ -> liftIO . print $ "p") $ (S.concatUnfold process) S.|$ srcs
      where
        -- S.finallyIO finalize $ 
        getLatest :: forall n a. (Ord n) => a -> (n, M.Map n a) -> (n, a)
        getLatest a' (n, a) = (n, fromMaybe a' (M.lookup n a))


bothUnfold :: forall m a b c d. (Monad m) => FL.Fold m a b -> FL.Fold m c d -> UF.Unfold m (a, c) (b, d)
bothUnfold f g = UF.teeZipWith (,) (foldUF fst f) (foldUF snd g)

foldUF :: forall m a b c. (Monad m) => (a -> c) -> FL.Fold m c b -> UF.Unfold m a b
foldUF fn fld = UF.singletonM (UF.fold (UF.singleton fn) fld)


demuxWithLatest :: forall m n a b. (Monad m) =>
            FL.Fold m (n, a) (M.Map n b)  -> FL.Fold m (n, a) (Maybe n, M.Map n b)
demuxWithLatest fo = f
  where
    f :: FL.Fold m (n, a) (Maybe n, M.Map n b)
    f = (,) <$> (FL.mkPureId ((const (Just . fst))) Nothing) <*> fo



eitherWalay :: [NodeMAC] -> FL.Fold GraphM (Either (NodeMAC, EnergyState) (NodeMAC, RuntimeStats)) ((Maybe NodeMAC, M.Map NodeMAC SensorR), (Maybe NodeMAC, M.Map NodeMAC (MeshNode, RxSignal)))
eitherWalay nodes = FL.partition
  (demuxWithLatest (FL.demux $ M.fromList $ zip nodes (repeat sensorFold)))
  (demuxWithLatest (FL.demux $ M.fromList $ zip nodes (repeat meshF)))


-- pfart :: Monad m => FL.Fold m b x -> FL.Fold m c y -> FL.Fold m (Either b c) (Either x y)
-- pfart (FL.Fold stepL beginL doneL) (FL.Fold stepR beginR doneR) = FL.mkFold step begin done
--   where
--     begin = do
--       resL <- beginL
--       resR <- beginR
--       return $ resL 
    

