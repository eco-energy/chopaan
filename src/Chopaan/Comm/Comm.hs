{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses, CPP #-}
module Chopaan.Comm.Comm (Chopaan.Comm.Dispatch.Dispatch(..)
                         , WriteChan(..)
                         , Chopaan.Comm.Address.Address(..)
                         , initQs
                         , initMessageQs
                         , initPubQ
                         , initPubQIO
                         , MessageQs(..)
                         , PubQueue
                         , writeToPubQ
                         , readPubQ
                         , mkCallback
                         --, mkCallback'
                         , trivialCallback
                         , subStream
                         , writeChan
                         , unfoldChan
                         ) where


import qualified Network.MQTT.Topic as MQ
import qualified Network.MQTT.Client as MQ
import Chopaan.Comm.Address


import qualified Data.ByteString.Lazy as BL
import Data.ProtoLens.TextFormat

import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift
import Control.Concurrent.STM
import Control.Concurrent (forkIO)

import Streamly (IsStream, MonadAsync)
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Stream.Parallel as S
import qualified Streamly.Internal.Data.Stream.IsStream as S

import Chopaan.Node.NodeId
import Chopaan.Comm.Queues
import qualified Control.Concurrent.Chan.Unagi as UC

import Chopaan.Comm.Dispatch
import Proto.NodeMessageSchema.NodeMessages hiding (Outgoing, Incoming)

{--------------------------------- Queue Implementation -----------------------------------}


type PubQueue = NodeQueue MQ.Topic MeshFrame


newtype WriteChan n a = WriteChan (UC.InChan (n, a))

type StateChan n = (Address n) => WriteChan n EnergyState

type StatsChan n = (Address n) => WriteChan n RuntimeStats

writeChan :: WriteChan n a -> n -> a -> IO ()
writeChan (WriteChan w) = curry $ UC.writeChan w

data MessageQs n = MessageQs
  { stateChan :: StateChan n
  , statsChan :: StatsChan n
  , outbox :: PubQueue
  }


unfoldChan :: (IsStream t, MonadAsync m) => WriteChan n a -> m (t m (n, a))
unfoldChan (WriteChan wc) = (\rc -> pure $ S.repeatM (liftIO $ UC.readChan rc))
                            =<< (liftIO . UC.dupChan $ wc) 
{-# INLINE unfoldChan #-}

initPubQ :: STM (PubQueue)
initPubQ = initNodeQueue

initPubQIO :: IO (PubQueue)
initPubQIO = atomically initPubQ

initQs :: IO (MessageQs NodeMAC)
initQs = initMessageQs @NodeMAC

initMessageQs :: forall n. (Address n, Show n) => IO (MessageQs n)
initMessageQs = do 
  (es, esR) <- UC.newChan -- @n @EnergyState ns
  (rs, rsR) <- UC.newChan -- @n @RuntimeStats ns
  _ <- forkIO $ incomingMonitor esR
  _ <- forkIO $ incomingMonitor rsR
  out <-  atomically $ initPubQ
  return $ MessageQs (WriteChan es) (WriteChan rs) out
  where
    incomingMonitor :: forall a. (Show n, Show a) => UC.OutChan (n, a) -> IO ()
    incomingMonitor ic = S.drain $ S.repeatM (UC.readChan ic) 


writeToPubQ :: (Dispatch a) => PubQueue -> MQ.Topic -> a -> IO ()
writeToPubQ p n et = atomically $ writeNodeQ p n (frame et)

readPubQ :: PubQueue -> IO (MQ.Topic, MeshFrame)
readPubQ = atomically . readNodeQ

trivialCallback :: MQ.MessageCallback
trivialCallback = MQ.SimpleCallback (\_ _ _ _ -> return ())

mkCallback :: forall n. (Address n) => MessageQs n -> MQ.MessageCallback
mkCallback (MessageQs { stateChan, statsChan })  = MQ.SimpleCallback $ writer
  where
    writer :: MQ.MQTTClient -> MQ.Topic -> BL.ByteString -> [MQ.Property] -> IO ()
    writer _ t msg _ = do
      --liftIO . print $ "Message Recieved"
      case nodeId of
        Nothing -> print $ "MQTT Topic Decode error: " <> (show t)
        (Just n) ->
          case parseDispatch msg of
            (Left err) -> error err
            (Right mf) -> do
              case (accessEnergyState mf) of
                (Just a) ->  writeChan stateChan n a
                Nothing -> case (accessRTS mf) of
                  (Just a) -> writeChan statsChan n a
                  Nothing -> print ("Not RTS AND NOT ES" <> showMessage mf) >> return ()
      where
        nodeId :: Maybe n
        nodeId = fromStateTopic $ t


-- mkCallback' :: forall t m n. (IsStream t, MonadAsync m, Address n, MonadUnliftIO m)
--             => m (MQ.MessageCallback, (t m (n, EnergyState), t m (n, RuntimeStats)))
-- mkCallback' = do
--   (meshCB, meshS) <- S.newCallbackStream
--   (gridCB, gridS) <- S.newCallbackStream
--   let
--     writer' :: MQ.MQTTClient -> MQ.Topic -> BL.ByteString -> [MQ.Property] -> m ()
--     writer' _ t msg _ = do
--       liftIO . print $ "Message Recieved"
--       case nodeId of
--         Nothing -> liftIO $ print $ "MQTT Topic Decode error: " <> (show t)
--         (Just n) ->
--           case parseDispatch msg of
--             (Left err) -> error err
--             (Right mf) -> do
--               case (accessEnergyState mf) of
--                 (Just a) -> gridCB (n, a)
--                 Nothing -> case (accessRTS mf) of
--                   (Just a) -> meshCB (n, a)
--                   Nothing -> liftIO $ print ("Not RTS AND NOT ES" <> showMessage mf) >> return ()
--       where
--         nodeId :: Maybe n
--         nodeId = fromStateTopic $ t
--   nt <- askRunInIO
--   let w a b c d = nt (writer' a b c d)
--       -- asS :: S.Stream m a -> t m a
--       -- asS = S.fromStreamS . S.toStreamK
--   return $ (MQ.SimpleCallback (w), (gridS, meshS))
-- {-# INLINE mkCallback' #-}



{------------------------- Streaming from Queues ---------------------------}

subStream :: forall t m n a. (IsStream t, MonadAsync m, Address n, Dispatch a) => n -> WriteChan n a -> m (t m a)
subStream n (WriteChan wc) = do
  --liftIO . print $ "subscribing to " <> show n 
  rc <- liftIO . UC.dupChan $ wc
  return $ S.map snd
   -- $ S.trace (liftIO . (\(n', _) -> print $ "After Filter " <> show n' <> "\n Expected " <> show n))
    $ S.filter (\(n', _) -> n' == n)
    -- $ S.trace (liftIO . (\(n', _) -> print $ "Before Filter " <> show n' <> "\n Expected " <> show n))
    $ S.repeatM . liftIO $ UC.readChan rc
{-# INLINE subStream #-}

