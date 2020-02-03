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
                 runNodeQueue,
                 queueStream,
                 KibbutzEvents(..),
                 writeToPubQ,
                 printQueueStream,
                 KibbutzMonitor,
                 initKibbutzMonitor,
                 updateKM) where


import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Lens.Micro
import qualified Data.Set as Set


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
import Control.Concurrent (forkIO)
import Streamly

import qualified Streamly.Prelude as S

import Brick.BChan (BChan, writeBChan)


-- Protobuf
import Data.ProtoLens.Encoding (decodeMessage)
import Data.ProtoLens (Message)

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State (put, MonadState, get, modify, runStateT)

import Data.Hashable (Hashable(..))
import qualified StmContainers.Map as SMap


type ThingName = Text.Text

type NodeT = (NodeId ThingName)

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

newtype NodeQueue a b = NodeQueue { runNodeQueue :: ((HasTopics a, Message b) => TQueue (a, b)) }

type PubQueue = NodeQueue NodeT EnergyTransactionRequest

type SubQueue = NodeQueue NodeT EnergyState

initNodeQ :: (HasTopics a, Message b) => STM (NodeQueue a b)
initNodeQ = do
  n <- newTQueue
  return $ NodeQueue n

writeToPubQ :: PubQueue -> NodeT -> EnergyTransactionRequest -> IO ()
writeToPubQ p n et = do
  atomically $ writeTQueue (runNodeQueue p) (n, et)

data Kibbutz = Kibbutz
  { kname :: Text.Text
  , nodes :: [NodeT]
  , inQueue :: SubQueue
  , outQueue :: PubQueue
  } deriving (Generic)


instance Show Kibbutz where
  show Kibbutz {..} = Text.unpack $ (kname <> " Kibbutz, " <> (Text.pack $ show $ length nodes) <> " nodes")

data KibbutzEvents = StateUpdate deriving (Eq, Ord, Show)

kbtz :: Text.Text -> [NodeT] -> SubQueue -> PubQueue -> Kibbutz
kbtz = Kibbutz

getKibbutz :: Text.Text -> IO Kibbutz
getKibbutz n = do
  ts <- getThings n
  let
    ns = map (mkNode. fromJust . thingName) ts
  iq <- atomically $ initNodeQ
  oq <- atomically $ initNodeQ
  return $ kbtz n ns iq oq

mkCallback :: Kibbutz -> BChan KibbutzEvents -> MQ.MessageCallback
mkCallback Kibbutz { inQueue } brickChan  = MQ.SimpleCallback $ writer
  where
    writer :: MQ.MQTTClient -> MQ.Topic -> BL.ByteString -> [MQ.Property] -> IO ()
    writer _ t msg _ = do
      print (nodeId, parsed)
      atomically $ do
        writeTQueue (runNodeQueue inQueue) (nodeId, parsed)
      writeBChan brickChan StateUpdate
      where
        nodeId :: NodeT
        nodeId = (fromJust . fromStateTopic) t
        parsed :: EnergyState
        parsed = ((fromRight defaultES) . decodeMessage . toStrict) msg
        toStrict = BS.concat . BL.toChunks

queueStream :: SubQueue -> SerialT IO (NodeT, EnergyState)
queueStream (NodeQueue q) = S.repeatM $ do return ((NodeId "a4:12:36:ss:cc" :: NodeT), defaultES) -- (atomically $ readTQueue q) --

printQueueStream :: SerialT IO (NodeT, EnergyState) -> IO ()
printQueueStream = S.mapM_ print


type KibbutzMonitor = SMap.Map NodeT NodeS

initKibbutzMonitor :: [NodeT] -> STM KibbutzMonitor
initKibbutzMonitor ns = do
    m <- SMap.new
    _ <- mapM (\n -> SMap.insert defNodeS n m) ns
    return m

updateKM :: KibbutzMonitor -> NodeT -> NodeS ->  STM ()
updateKM m n v = do
  SMap.insert v n m


-- foldMapWith :: (IsStream t, Foldable f) => (t m b -> t m b -> t m b) -> (a -> t m b) -> f a -> t m b
runKibbutzMonitor :: SMap.Map NodeT NodeS -> f SerialT (STM (NodeT, NodeS)) -> SerialT STM (SMap.Map NodeT NodeS)
runKibbutzMonitor km st = do undefined
    -- foldMapWith (S.mapM_ (\(m, v)-> updateKM km m v)) st

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
  env <- newEnv Discover <&> set envRegion Singapore <&> configure iiot -- set envLogger lgr . 
  runResourceT . runAWST env $ do
    things <- send (Iot.listThings & Iot.ltThingTypeName .~ ttn)
    return $ things ^. Iot.ltrsThings
