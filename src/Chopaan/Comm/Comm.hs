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
{-# LANGUAGE MultiParamTypeClasses #-}
module Chopaan.Comm.Comm where

import qualified Network.MQTT.Topic as MQ
import qualified Network.MQTT.Client as MQ
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Hashable

import Lens.Micro hiding (_Just)
import Proto.NodeMessageSchema.NodeMessages hiding (Outgoing, Incoming)
import Proto.NodeMessageSchema.NodeMessages_Fields
import Data.ProtoLens.TextFormat
import Data.ProtoLens.Labels()

import Data.ProtoLens
import Data.ProtoLens.Prism

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM
import Control.Concurrent (forkIO)

import Streamly
import qualified Streamly.Prelude as S

import Chopaan.Node.NodeId
import Chopaan.Kibbutz.AWS.Things
import Chopaan.Comm.Queues
import qualified Control.Concurrent.Chan.Unagi as UC

import Data.Text (Text)

{--------------------- Type Classes for message conversion and addressing --------------------------}

class (Show a) => Dispatch a where
  frame :: a -> MeshFrame
  unframe :: MeshFrame -> Maybe a


instance Dispatch EnergyState where
  frame es = defMessage & maybe'payload .~ (_Just # _MeshFrame'State # es) 
  unframe = accessEnergyState

instance Dispatch RuntimeStats where
  frame es = defMessage & maybe'payload .~ (_Just # _MeshFrame'RtStats # es) 
  unframe = accessRTS

instance Dispatch EnergyTransactionRequest where
  frame etr = defMessage & maybe'payload .~ (_Just # _MeshFrame'NodeTxRequest # etr) 
  unframe = accessETR

instance Dispatch HardwareConfig where
  frame hwc = defMessage & maybe'payload .~ (_Just # _MeshFrame'Hw # hwc)
  unframe = accessHWConf

instance Dispatch NodeControl where
  frame nc = defMessage & maybe'payload .~ (_Just # _MeshFrame'Control # nc)
  unframe = accessNodeControl

instance Dispatch Transaction where
  frame tx = defMessage & maybe'payload .~ (_Just # _MeshFrame'Transaction # tx) 
  unframe = accessTx


class (Ord a, Hashable a, Show a) => Address a where
  stateTopic :: a -> MQ.Topic
  controlTopic :: a -> MQ.Topic
  logTopic :: a -> MQ.Topic
  fromControlTopic :: MQ.Topic -> Maybe a
  fromStateTopic :: MQ.Topic -> Maybe a
  fromLogTopic :: MQ.Topic -> Maybe a
  toRemoteId :: a -> Text
  rootTopic :: a -> MQ.Topic



instance Address (NodeMAC) where
  stateTopic = (nameToTopic "/state") . unNodeId
  controlTopic = (nameToTopic "/control" )  . unNodeId
  logTopic = (nameToTopic "/logs") . unNodeId
  fromControlTopic = topicToNodeId "/control"
  fromStateTopic = topicToNodeId "/state"
  fromLogTopic = topicToNodeId "/logs"
  toRemoteId = unNodeId
  rootTopic _ = "/kibbutz/node/root"


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


initPubQ :: STM (PubQueue)
initPubQ = initNodeQueue @MQ.Topic @MeshFrame

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


class (Address n) => Subscribe n a where
  subscribe :: n -> NodeQueue n a


data Incoming a = Incoming a deriving (Eq, Ord, Show, Functor)
data Outgoing a = Outgoing a deriving (Eq, Ord, Show, Functor)

instance (Dispatch a) => Dispatch (Outgoing a) where
  frame (Outgoing a) = frame a
  unframe a = Outgoing <$> unframe a


instance (Dispatch a) => Dispatch (Incoming a) where
  frame (Incoming a) = frame a
  unframe a = Incoming <$> unframe a

  
instance Dispatch MeshFrame where
  frame = id
  unframe = Just . id


trivialCB :: MQ.MessageCallback
trivialCB = MQ.SimpleCallback (\_ _ _ _ -> return ())

mkCallback :: forall n. (Address n) => MessageQs n -> MQ.MessageCallback
mkCallback (MessageQs { stateChan, statsChan })  = MQ.SimpleCallback $ writer
  where
    writer :: MQ.MQTTClient -> MQ.Topic -> BL.ByteString -> [MQ.Property] -> IO ()
    writer _ t msg _ = do
      case nodeId of
        Nothing -> print $ "MQTT Topic Decode error: " <> (show t)
        (Just n) ->
          case parsed of
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
        parsed :: Either String MeshFrame
        parsed = (decodeMessage . toStrict) msg
        toStrict = BS.concat . BL.toChunks


class (Address n, Dispatch a) => Comm n a where
  

{------------------------- Streaming from Queues ---------------------------}

subStream :: forall t m n a. (IsStream t, MonadAsync m, Address n, Dispatch a) => n -> WriteChan n a -> m (t m a)
subStream n (WriteChan wc) = do
  liftIO . print $ "subscribing to " <> show n 
  rc <- liftIO . UC.dupChan $ wc
  return $ S.map snd
    -- $ S.trace (liftIO . (\(n', _) -> print $ "After Filter " <> show n' <> "\n Expected " <> show n))
    $ S.filter (\(n', _) -> n' == n)
    -- $ S.trace (liftIO . (\(n', _) -> print $ "Before Filter " <> show n' <> "\n Expected " <> show n))
    $ S.repeatM . liftIO $ UC.readChan rc


{---------------------------- Utils -------------------------------------}

fromPayload m l = m ^? maybe'payload . _Just . l

accessEnergyState :: MeshFrame -> Maybe EnergyState
accessEnergyState m = fromPayload m _MeshFrame'State
                      
accessETR :: MeshFrame -> Maybe EnergyTransactionRequest
accessETR m = fromPayload m _MeshFrame'NodeTxRequest

accessRTS :: MeshFrame -> Maybe RuntimeStats
accessRTS m = fromPayload m _MeshFrame'RtStats

accessHWConf :: MeshFrame -> Maybe HardwareConfig
accessHWConf m = fromPayload m _MeshFrame'Hw

accessNodeControl :: MeshFrame -> Maybe NodeControl
accessNodeControl m = fromPayload m _MeshFrame'Control

accessTx :: MeshFrame -> Maybe Transaction
accessTx m = fromPayload m _MeshFrame'Transaction
