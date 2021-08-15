{-# LANGUAGE TypeApplications, FlexibleContexts, ScopedTypeVariables, RankNTypes, ConstraintKinds, KindSignatures, QuantifiedConstraints, MultiParamTypeClasses, GADTs, FlexibleInstances#-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns  #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, DerivingStrategies, DeriveAnyClass, DeriveFunctor, StandaloneDeriving, TupleSections, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chopaan.Kibbutz where

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
import qualified Data.Map as M
import Data.Maybe
import Data.Bifunctor (bimap)
import Data.Greskell (runBinder)
import NetSpider.Graph (NodeAttributes(..))
import Control.Concurrent (forkIO)
import Control.Lens

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
import Chopaan.Node.Mesh (MeshNode, RxSignal, meshNodeLink, meshF, sigToFN)
import Chopaan.Utils.Time


import Chopaan.Comm.Mqtt (runMqtt)
import Chopaan.Comm.S3
import Chopaan.Comm.Comm (MessageQs(..)
                         , Address(..)
                         , mkCallback
                         , mkCallback'
                         , PubQueue
                         , unfoldChan
                         , initMessageQs
                         )
import Chopaan.Graph.Kbtz
import Chopaan.Graph
import Data.Time
import Data.Text (pack)

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



type S3S (t :: (* -> *) -> * -> *) m a b = t m ((NodeMAC, ObjectKey, Maybe UTCTime), Either (NodeMAC, a) (NodeMAC, b))


s3Stream :: (IsStream t, MonadAsync m, MonadCatch m)
         => S3Opts
         -> [(NodeMAC, Maybe ObjectKey)]
         -> (UTCTime, UTCTime)
         -> S3S t m EnergyState RuntimeStats
s3Stream bucket ns range = S.tapRate 10 (liftIO . (print . (prefix <>) . show))
                           $ S.mapM (pure . (second f) . align)
                           $ S.concatMapWith S.parallel (uncurry (nodeS3 bucket range))
                           $ S.fromList ns
  where
    prefix = "combined rate: "
    -- (S.mergeBy onTime)
    onTime (_, ((_, a), _)) (_, ((_, b), _)) = fromMaybe EQ $ liftA2 compare a b
    align (a, ((b, c), d)) = ((b, a, c), (b, d)) 
    f (n, c) = case c of
      Left x -> Left (n, x)
      Right y -> Right (n, y)


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

--mqttSrc :: forall t m. (KbtzConn t m NodeMAC) => KbtzName -> [NodeMAC] -> MQTTOpts
--  -> m ((t m (NodeMAC, EnergyState), t m (NodeMAC, RuntimeStats), PubQueue))
--mqttSrc k ns o = (qSrc qs)  =<< (mqttQs qs o k ns)



runKibbutz' :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
  => DBPools -> KbtzC NodeMAC -> t m (KbtzScene NodeMAC)
runKibbutz' poo = S.concatM . (runKibbutzM poo)

runKibbutzM :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
  => DBPools -> KbtzC NodeMAC -> m (t m (KbtzScene NodeMAC))
runKibbutzM poo = (pure . S.adapt . S.hoist (runGraphWithDB poo)) <=< (runGraphWithDB poo . runKibbutz)


type GridScene n = ((NodeStates n, Maybe (TxPlan n)), TxState n)

type MeshScene n = (n, (MeshNode, RxSignal))
type KbtzScene n = Either (GridScene n) (MeshScene n)


runKibbutz :: forall t. (IsStream t) => KbtzC NodeMAC -> GraphM (t GraphM (KbtzScene NodeMAC))
runKibbutz kc@KbtzC{name, nodes, channelOpts} = do
  -- Live Data
  liftIO . print $ show kc
  t0 <- liftIO $ getCurrentTime 
  (es, rs, outbox) <- case channelOpts of
    (Left mqopts) -> do
      qs <- liftIO initMessageQs
      (mqclient, (es, rs)) <- mqttStreams qs mqopts (fmap (<> "_" <> (pack . show $ t0)) name) nodes
      mq <- toIO $ mqclient ()
      liftIO . forkIO $ mq
      return (es, rs, (outbox qs))
    (Right qs) -> qSrc qs  
  
  gridFold <- withSpider $ saveTx name
  meshFold <- withSpider addMeshNode
  -- Stream Processors that run Folds
  let
    processES :: t GraphM (NodeMAC, EnergyState) -> t GraphM (GridScene NodeMAC)
    processES s = tapCount "ES: " --S.tapRate 30 (\x -> liftIO . print $ "Grid Processed Rate: " <> show x)
                S.|$ S.map snd
                S.|$ S.tap (FL.lmap getLatest gridFold)
                S.|$ status
                --S.|$ S.trace (dispatchTxSafe outbox . snd . snd)
                S.|$ plan
                S.|$ S.mapM (pure . second Tx)
                S.|$ gridSensorR nodes
                $ s
                -- $ S.tapRate 30 (\x -> liftIO . print $ "Grid Incoming Rate: " <> show x) s

    processRS s = tapCount "RS: " -- S.tapRate 30 (\x -> liftIO . print $ "Mesh Processed Rate: " <> show x)
                . S.tap meshFold
                . S.map (second (meshNodeLink $ getGridRoot name))
                $ s
                -- $ S.tapRate 30 (\x -> liftIO . print $ "Mesh Incoming Rate: " <> show x) s
  let
    liveStream = (Left <$> (processES es)) `S.parallel` (Right <$> (processRS rs)) 
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
    printCount s = FL.foldlM' (\x a ->
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
                   . S.postscan (FL.tee (FL.foldl' ((const (Just . fst))) Nothing)
                               sensorFD) $ s
  where
    sensorFD = FL.demux $ M.fromList $ (, sensorFold) <$> ns
    {-# INLINE sensorFD #-}
{-# INLINE gridSensorR #-}

hydrateKbtz' :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
  => DBPools -> KbtzC NodeMAC -> Range -> t m Hydration
hydrateKbtz' poo k = S.concatM . (hydrateKbtzM poo k)

hydrateKbtzM :: forall t m. (IsStream t, MonadAsync m, MonadCatch m)
  => DBPools -> KbtzC NodeMAC -> Range -> m (t m Hydration)
hydrateKbtzM poo k = (pure . S.adapt . S.hoist (runGraphWithDB poo))
                   <=< (runGraphWithDB poo . hydrateKbtz k)

type Hydration = ((NodeMAC, ObjectKey, Maybe UTCTime)
                 , ((M.Map NodeMAC SensorR)
                   , (M.Map NodeMAC (MeshNode, RxSignal))))

type Range = (UTCTime, UTCTime)

hydrateKbtz :: forall t. (IsStream t) => KbtzC NodeMAC -> Range -> GraphM (t GraphM (Hydration))
hydrateKbtz KbtzC{name, nodes, s3Opts} range = case s3Opts of
      Nothing -> return $ S.nil
      Just bucket -> do
        -- x <- liftIO $ newTVarIO (M.fromList [])
        -- lsyncs <- mapM (\n -> do
        --                    ls <- withKbtzPool (flip getNodeLastSync n) 
        --                    return $ (\a -> (n, ObjectKey <$> a)) (listToMaybe ls)
        --                ) nodes
        let
          lsyncs = zip nodes (repeat Nothing)
          srcs :: t GraphM ((NodeMAC, ObjectKey, Maybe UTCTime), Either (NodeMAC, EnergyState) (NodeMAC, RuntimeStats))
          srcs = S.mapM (pure . fixMeshTS) $ s3Stream bucket lsyncs range
                 --where
                 --  ml = meshNodeLink (getGridRoot name)
            -- where
            -- S.trace (\((_, _, t), a) -> case a of
            --                  Left _ -> return ()
            --                  Right (n, r) -> do
            --                    liftIO . print $ n
            --                    liftIO . print $ r
            --                    liftIO . print $ t 
            --                    liftIO . print $ ml r
            --                    liftIO . print $ sigToFN (n, (ml r))
            --                  ) $ 
            --   thatF :: (NodeMAC, ObjectKey, Maybe UTCTime) -> GraphM ()
            --   thatF (n, k, t) = liftIO . atomically $ modifyTVar' x (M.insert n (k, t))
          
          process :: FL.Fold GraphM ((NodeMAC, ObjectKey, Maybe UTCTime), Either (NodeMAC, EnergyState) (NodeMAC, RuntimeStats)) Hydration
          process = secondF (eitherWalay name nodes (fSave name) (mSave))
          
        -- let finalize = do
        --       m <- liftIO . atomically $ readTVar x
        --       mapM_ (\(n, ((ObjectKey k), _)) ->
        --                                withKbtzPool (\p -> addLastSyncToHH p n k)) $ M.toList m
        --let f = UF.concat (UF.fromStream S.postscan process) saveAndIdWithKey
        return $ S.tapRate 10 (liftIO . (print . (prefix <>) . show))
                                               $ (S.postscan process) S.|$ srcs
        where
          prefix = "processing rate: "
          fixMeshTS :: ((NodeMAC, ObjectKey, Maybe UTCTime)
                       , Either (NodeMAC, EnergyState) (NodeMAC, RuntimeStats))
            -> ((NodeMAC, ObjectKey, Maybe UTCTime)
               , Either (NodeMAC, EnergyState) (NodeMAC, RuntimeStats))
          fixMeshTS a@(_, (Left _)) = a
          fixMeshTS a@((_, _, Nothing), _) = a
          fixMeshTS a@((n, o, Just t), Right (n', r)) = case r ^? N.cpuTime of
            Nothing -> ((n, o, Just t), Right (n', r & N.cpuTime .~ (timeToUIntSeconds t)))
            (Just t') -> case (t' == 0) of
              True -> ((n, o, Just t), Right (n', r & N.cpuTime .~ (timeToUIntSeconds t)))
              False -> a


bothUnfold :: forall m a b c d. (Monad m) => FL.Fold m a b -> FL.Fold m c d -> UF.Unfold m (a, c) (b, d)
bothUnfold f g = UF.zipWith (,) (foldUF fst f) (foldUF snd g)

foldUF :: forall m a b c. (Monad m) => (a -> c) -> FL.Fold m c b -> UF.Unfold m a b
foldUF fn fld = UF.functionM (UF.fold fld (UF.function fn))


demuxWithLatest :: forall m n a b. (Monad m) =>
            FL.Fold m (n, a) (M.Map n b)  -> FL.Fold m (n, a) (n, M.Map n b)
demuxWithLatest fo = FL.lmap (\(n, a) -> (n, (n, a))) f
  where
    f :: FL.Fold m (n, (n, a)) (n, M.Map n b)
    f = secondF fo

fSave k n = (\x -> withSpider $ do
                a <- addFlow k (n, x)
                b <- addMon k (n, (x, Nothing))
                c <- addTx k (n, (x, Nothing, Nothing))
                return ((a && b && c) `seq` x )
          )

mSave n = (\x -> do
              -- a <- withSpider $ addMeshN (n, x)
              -- case a of
              --   True -> liftIO $ print "Mesh Added No Exception!"
              --   False -> liftIO $ print ("Add Mesh Failed on" <> show x)
              return x
          )

eitherWalay :: KbtzName
            -> [NodeMAC]
            -> (NodeMAC -> SensorR -> GraphM SensorR)
            -> (NodeMAC -> (MeshNode, RxSignal) -> GraphM (MeshNode, RxSignal))
            -> FL.Fold GraphM (Either (NodeMAC, EnergyState) (NodeMAC, RuntimeStats)) ((M.Map NodeMAC SensorR), (M.Map NodeMAC (MeshNode, RxSignal)))
eitherWalay k nodes fSave mSave = FL.partition
  ((FL.demux $ M.fromList $ fmap (\n -> (n, FL.rmapM (fSave n) sensorFold)) nodes))
  ((FL.demux $ M.fromList $ fmap (\n -> (n, FL.rmapM (mSave n) (meshF $ getGridRoot k))) nodes))

-- pfart :: Monad m => FL.Fold m b x -> FL.Fold m c y -> FL.Fold m (Either b c) (Either x y)
-- pfart (FL.Fold stepL beginL doneL) (FL.Fold stepR beginR doneR) = FL.mkFold step begin done
--   where
--     begin = do
--       resL <- beginL
--       resR <- beginR
--       return $ resL 
    

