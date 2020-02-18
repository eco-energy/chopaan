{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Registry (NodeT,
                 HasTopics(..),
                 getKibbutz,
                 Kibbutz(..),
                 mkCallback,
                 PubQueue,
                 SubQueue,
                 queueStream,
                 KibbutzEvents(..),
                 writeToPubQ,
                 printQueueStream,
                 nodeStream,
                 getMonitorState,
                 updateMonitorState,
                 defKName,
                 isTQEmpty,
                 NodeQueue(..),
                 initNodeQ,
                 writeToNodeQ) where


import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Lens.Micro


-- AWS Imports
import qualified Network.AWS.IoT.ListThings as Iot
import qualified Network.AWS.IoT.Types as Iot
import Control.Monad.Trans.AWS
import Data.Maybe
import Data.Either
import System.IO
import qualified Network.MQTT.Topic as MQ
import qualified Network.MQTT.Client as MQ
import Node
import Proto.NodeMessages

-- STM
import Control.Concurrent.STM
import Streamly

import qualified Streamly.Prelude as S
import qualified Data.Time as Time 


-- Protobuf
import Data.ProtoLens.Encoding (decodeMessage)
import Data.ProtoLens (Message)


import StateMonitor



mkNode :: ThingName -> NodeT
mkNode = NodeId

class HasTopics a where
  fromThingAttr :: Iot.ThingAttribute -> Maybe a
  stateTopic :: a -> MQ.Topic
  controlTopic :: a -> MQ.Topic
  fromControlTopic :: MQ.Topic -> Maybe a
  fromStateTopic :: MQ.Topic -> Maybe a

instance HasTopics (NodeT) where
  fromThingAttr a = fmap (NodeId) (thingName a)
  stateTopic = (nameToTopic "/state") . unNodeId
  controlTopic = (nameToTopic "/control" )  . unNodeId
  fromControlTopic = topicToNodeId "/control"
  fromStateTopic = topicToNodeId "/state"

newtype NodeQueue a b = NodeQueue { runNodeQueue :: ((HasTopics a, Message b) => TBQueue (a, b)) }

type PubQueue = NodeQueue NodeT EnergyTransactionRequest

type SubQueue = NodeQueue NodeT EnergyState

initNodeQ :: (HasTopics a, Message b) => STM (NodeQueue a b)
initNodeQ = do
  n <- newTBQueue 20
  return $ NodeQueue n


writeToNodeQ :: (HasTopics a, Message b) => NodeQueue a b -> a -> b -> IO ()
writeToNodeQ q topic msg = atomically $ writeTBQueue (runNodeQueue q) (topic, msg) 

writeToPubQ :: PubQueue -> NodeT -> EnergyTransactionRequest -> IO ()
writeToPubQ p n et = do
  atomically $ writeTBQueue (runNodeQueue p) (n, et)

data Kibbutz = Kibbutz
  { kname :: Text.Text
  , nodes :: [NodeT]
  , inQueue :: SubQueue
  , outQueue :: PubQueue
  , msgCount :: TVar Int
  } deriving (Generic)


isTQEmpty :: Kibbutz -> STM (Bool)
isTQEmpty Kibbutz {inQueue} = isEmptyTBQueue . runNodeQueue $ inQueue 

instance Show Kibbutz where
  show Kibbutz {..} = Text.unpack $ (kname <> " Kibbutz, " <> (Text.pack $ show $ length nodes) <> " nodes")

data KibbutzEvents = StateUpdate deriving (Eq, Ord, Show)

kbtz :: Text.Text -> [NodeT] -> SubQueue -> PubQueue -> TVar Int -> Kibbutz
kbtz = Kibbutz

getKibbutz :: Text.Text -> IO Kibbutz
getKibbutz n = do
  ts <- getThings n
  let
    ns = map (mkNode. fromJust . thingName) ts
  iq <- atomically $ initNodeQ
  oq <- atomically $ initNodeQ
  mc <- newTVarIO 0
  return $ kbtz n ns iq oq mc

mkCallback :: Kibbutz -> MQ.MessageCallback
mkCallback Kibbutz { inQueue, msgCount }  = MQ.SimpleCallback $ writer
  where
    writer :: MQ.MQTTClient -> MQ.Topic -> BL.ByteString -> [MQ.Property] -> IO ()
    writer _ t msg _ = do
      atomically $ do
        writeTBQueue (runNodeQueue inQueue) (nodeId, parsed)
        modifyTVar' msgCount (\a -> a + 1)
      where
        nodeId :: NodeT
        nodeId = (fromJust . fromStateTopic) t
        parsed :: EnergyState
        parsed = ((fromRight defaultES) . decodeMessage . toStrict) msg
        toStrict = BS.concat . BL.toChunks

queueStream :: (IsStream t) => SubQueue -> t IO (NodeT, EnergyState)
queueStream (NodeQueue q) = S.repeatM $ (atomically $ readTBQueue q)

printQueueStream :: SerialT IO (NodeT, EnergyState) -> IO ()
printQueueStream = S.mapM_ print

--nodeStream :: (IsStream t, Monad (t IO)) => Time.UTCTime -> [NodeT] -> ZipSerialM IO (NodeT, EnergyState) -> SerialT IO (NodeT, NodeS)
nodeStream :: (IsStream t, Monad m, Eq a) => Time.UTCTime -> [a] -> ZipSerialM m (a, EnergyState) -> t m (a, NodeMetrics WattSeconds Watts)
nodeStream initTime nodes q = serially $ foldr (<>) (go n) $ map (\n'-> go n') ns
  where (n:ns) = nodes
        go n' = S.zipWith (,) (S.repeat n') (runNodeMonitor initTime n' q)


defKName :: KibbutzName
defKName = Text.pack "kibbutz-pilot-node"

type KibbutzName = Text.Text

nameToTopic :: Text.Text -> ThingName -> MQ.Topic
nameToTopic suffix name = prefix <> n <> suffix
  where
    prefix = "/kibbutz/node/"
    n = Text.replace ":" "" name

topicToNodeId :: Text.Text -> Text.Text -> Maybe NodeT
topicToNodeId suffix t =
  case (isValidTopic t) of
       False  -> Nothing
       _ -> Just (NodeId n)
  where
    n = colonize $ Text.replace suffix "" $ Text.replace prefix "" t
    isValidTopic t' = prefix `Text.isPrefixOf` t' && suffix `Text.isSuffixOf` t'
    prefix = "/kibbutz/node/"
    colonize :: Text.Text -> Text.Text
    colonize cs = Text.intercalate i $ Text.chunksOf 2 cs
      where
        i = ":"
    -- TODO : Add back the colons!

thingName :: Iot.ThingAttribute -> Maybe ThingName
thingName t = t ^. Iot.taThingName

getThings :: Text.Text -> IO [Iot.ThingAttribute]
getThings thingTypeName = do
  let
    iiot = Iot.ioT{_svcPrefix="execute-api"} :: Service
    ttn = (Just thingTypeName) :: Maybe Text.Text
  lgr <- newLogger Trace stdout
  env <- newEnv Discover <&> set envLogger lgr . set envRegion Singapore <&> configure iiot --  
  runResourceT . runAWST env $ do
    things <- send (Iot.listThings & Iot.ltThingTypeName .~ ttn)
    return $ things ^. Iot.ltrsThings
