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

import GHC.Generics
-- STM
import Control.Concurrent.STM

import qualified Network.MQTT.Topic as MQ
import qualified Network.MQTT.Client as MQ
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

import Lens.Micro hiding (_Just)
import Proto.NodeMessageSchema.NodeMessages hiding (Outgoing, Incoming)
import Proto.NodeMessageSchema.NodeMessages_Fields
import Data.ProtoLens.TextFormat
import Data.ProtoLens.Labels()
import Data.Map.Strict as M


import Data.ProtoLens
import Data.ProtoLens.Prism

import Control.Monad.IO.Class (liftIO)
import Control.Monad ((<=<), (>=>))

import Streamly
import qualified Streamly.Prelude as S

import Chopaan.Node.NodeId
import Chopaan.Kibbutz.AWS.Things


{--------------------- Type Classes for message conversion and addressing --------------------------}

class Dispatch a where
  frame :: a -> MeshFrame
  unframe :: MeshFrame -> Maybe a


instance Dispatch EnergyState where
  frame es = defMessage & maybe'payload .~ (_Just # _MeshFrame'State # es) 
  unframe = accessEnergyState

instance Dispatch RuntimeStats where
  frame es = defMessage & maybe'payload .~ (_Just # _MeshFrame'RtStats # es) 
  unframe = accessRTS

instance Dispatch EnergyTransactionRequest where
  frame etr = defMessage & maybe'payload .~ (_Just # _MeshFrame'Transaction # etr) 
  unframe = accessETR

instance Dispatch HardwareConfig where
  frame hwc = defMessage & maybe'payload .~ (_Just # _MeshFrame'Hw # hwc)
  unframe = accessHWConf

instance Dispatch NodeControl where
  frame nc = defMessage & maybe'payload .~ (_Just # _MeshFrame'Control # nc)
  unframe = accessNodeControl


class (Ord a) => Address a where
  stateTopic :: a -> MQ.Topic
  controlTopic :: a -> MQ.Topic
  logTopic :: a -> MQ.Topic
  fromControlTopic :: MQ.Topic -> Maybe a
  fromStateTopic :: MQ.Topic -> Maybe a
  fromLogTopic :: MQ.Topic -> Maybe a


instance Address (NodeMAC) where
  stateTopic = (nameToTopic "/state") . unNodeId
  controlTopic = (nameToTopic "/control" )  . unNodeId
  logTopic = (nameToTopic "/logs") . unNodeId
  fromControlTopic = topicToNodeId "/control"
  fromStateTopic = topicToNodeId "/state"
  fromLogTopic = topicToNodeId "/logs"



{--------------------------------- Queue Implementation -----------------------------------}


class (Address n) => Subscribe n a where
  subscribe :: n -> NodeQueue n a

newtype NodeQueue a b = NodeQueue { runNodeQueue :: TBQueue (a, b) } deriving (Eq, Generic)


initNodeQueue :: forall a b. (Address a, Dispatch b) => STM (NodeQueue a b)
initNodeQueue = do
  n <- newTBQueue 10
  return $ NodeQueue n


type PubQueue n a = (Address n, Dispatch a) => NodeQueue n a

type StateQueues n = (Address n) => M.Map n (NodeQueue n EnergyState)

type StatsQueues n = (Address n) => M.Map n (NodeQueue n RuntimeStats)


data MessageQs n a = MessageQs
  { stateQs :: StateQueues n
  , statsQs :: StatsQueues n
  , outbox :: PubQueue n a 
  }
  

data Incoming a = Incoming a deriving (Functor)
data Outgoing a = Outgoing a deriving (Functor)

instance (Dispatch a) => Dispatch (Outgoing a) where
  frame (Outgoing a) = frame a
  unframe a = Outgoing <$> unframe a


instance (Dispatch a) => Dispatch (Incoming a) where
  frame (Incoming a) = frame a
  unframe a = Incoming <$> unframe a

  
instance Dispatch MeshFrame where
  frame = id
  unframe = Just . id

initMessageQs :: forall n o. (Address n, Dispatch o) => [n] -> STM (MessageQs n o)
initMessageQs ns = do
  es <- sequence $ M.fromList $ [(n, initNodeQueue @n @EnergyState) | n <- ns]
  rs <- sequence $ M.fromList $ [(n, initNodeQueue @n @RuntimeStats) | n <- ns] -- initNodeQueue @n @RuntimeStats <$> ns
  out <-  initNodeQueue @n @o
  return $ MessageQs es rs out

initQs = initMessageQs @NodeMAC @MeshFrame

writeToNodeQ :: forall a b. (Address a, Dispatch b) => NodeQueue a b -> a -> b -> IO ()
writeToNodeQ q n m = do
  atomically $ writeTBQueue (runNodeQueue q) (n, m)


writeToPubQ :: (Address n, Dispatch a) => PubQueue n a -> n -> a -> IO ()
writeToPubQ p n et = do
  atomically $ writeTBQueue (runNodeQueue p) (n, et)


trivialCB :: MQ.MessageCallback
trivialCB = MQ.SimpleCallback (\_ _ _ _ -> return ())

mkCallback :: forall n a. (Address n, Dispatch a) => MessageQs n a -> MQ.MessageCallback
mkCallback (MessageQs { stateQs, statsQs })  = MQ.SimpleCallback $ writer
  where
    writer :: MQ.MQTTClient -> MQ.Topic -> BL.ByteString -> [MQ.Property] -> IO ()
    writer _ t msg _ = do
      case nodeId of
        Nothing -> print $ "MQTT Topic Decode error: " <> (show t)
        (Just n) ->
          case parsed of
            (Left err) -> error err
            (Right mf) -> case (accessEnergyState mf) of
              (Just a) ->  writeToNodeQ @_ @EnergyState (stateQs M.! n) n a -- print ("GOT ENERGY STATE:\n" <> showMessage a) >>
              Nothing -> case (accessRTS mf) of
                (Just a) -> writeToNodeQ @_ @RuntimeStats (statsQs M.! n) n a -- print ("GOT RTS:\n" <> showMessage a) >>
                Nothing -> print ("Not RTS AND NOT ES" <> showMessage mf) >> return ()
      where
        nodeId :: Maybe n
        nodeId = fromStateTopic $ t
        parsed :: Either String MeshFrame
        parsed = (decodeMessage . toStrict) msg
        toStrict = BS.concat . BL.toChunks


class (Address n, Dispatch a) => Comm n a where
  

{------------------------- Streaming from Queues ---------------------------}

subStream :: forall t m n a. (IsStream t, MonadAsync m, Address n, Dispatch a) => n -> NodeQueue n a -> t m a
subStream n sq = S.map snd $ S.repeatM $ liftIO $ (atomically $ readTBQueue . runNodeQueue $ sq)


{---------------------------- Utils -------------------------------------}

fromPayload m l = m ^? maybe'payload . _Just . l

accessEnergyState :: MeshFrame -> Maybe EnergyState
accessEnergyState m = fromPayload m _MeshFrame'State
                      
accessETR :: MeshFrame -> Maybe EnergyTransactionRequest
accessETR m = fromPayload m _MeshFrame'Transaction

accessRTS :: MeshFrame -> Maybe RuntimeStats
accessRTS m = fromPayload m _MeshFrame'RtStats

accessHWConf :: MeshFrame -> Maybe HardwareConfig
accessHWConf m = fromPayload m _MeshFrame'Hw

accessNodeControl :: MeshFrame -> Maybe NodeControl
accessNodeControl m = fromPayload m _MeshFrame'Control
