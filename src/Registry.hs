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
module Registry (getThings, HasTopics(..), NodeT, NodeQueue, ThingName, getKibbutz, Kibbutz(..), mkCallback, PubQueue, SubQueue, runNodeQueue, queueStream) where


import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Data.Data
import Lens.Micro
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Text.PrettyPrint.Tabulate as PPT

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
import Proto.NodeMessages_Fields

-- STM
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue

import Streamly

import qualified Streamly.Prelude as S
import Control.Monad (replicateM)



-- Protobuf
import Proto.NodeMessages
import Proto.NodeMessages_Fields
import Data.ProtoLens.Encoding (decodeMessage, encodeMessage)
import Data.ProtoLens (Message, defMessage)


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

type PubQueue = NodeQueue NodeT MeshFrame

type SubQueue = NodeQueue NodeT EnergyState

initNodeQ :: (HasTopics a, Message b) => STM (NodeQueue a b)
initNodeQ = do
  n <- newTQueue
  return $ NodeQueue n

{--
newtype KibbutzState a = KibbutzState { unKibbutzState :: Map.Map NodeT a } deriving (Eq, Generic)

type KibbutzStateT = KibbutzState (NodeQueue)

initKibbutzStateT :: Set.Set NodeT -> STM (KibbutzStateT)
initKibbutzStateT ns = do
  qs <- replicateM (length ns) initNodeQ
  return $ KibbutzState $ Map.fromList $ zip (Set.toList ns) qs
--}
-- window all the scanl fns

{--
printAudit :: KibbutzState -> IO ()
printAudit ns = PPT.printTable $ unKibbutzState ns
--}


data Kibbutz = Kibbutz
  { kname :: Text.Text
  , nodes :: Set.Set NodeT
  , inQueue :: SubQueue
  , outQueue :: PubQueue
  } deriving (Generic)

{--
foldQueue :: NodeQueue a b -> (x -> a -> IO x) -> IO x -> (x -> IO b) -> IO b
foldQueue q step start done =
  let go state =
        do m <- atomically (readTQueue queue)
           case m of
             Nothing -> done state
             Just a -> step state a >>= go
--}
{--runKibbutz :: _
runKibbutz Kibbutz {..} = do
  let getES' = atomically do
        es <- readTQueue $ runNodeQueue inQueue
        es >>= getES
      s = S.foldr (S.|:) S.nil (getES)
  return 
  --S.map (\n-> runNodeMonitor n es) $ (S.fromList . Set.toList) nodes
  --forever $ runMqtt
--}  

instance Show Kibbutz where
  show Kibbutz {..} = Text.unpack $ (kname <> " Kibbutz, " <> (Text.pack $ show $ length nodes) <> " nodes")

kbtz = Kibbutz

getKibbutz :: Text.Text -> IO Kibbutz
getKibbutz n = do
  ts <- getThings n
  let
    ns = Set.fromList $ map (mkNode. fromJust . thingName) ts
  iq <- atomically $ initNodeQ
  oq <- atomically $ initNodeQ
  return $ kbtz n ns iq oq

mkCallback :: Kibbutz -> MQ.MessageCallback
mkCallback Kibbutz { inQueue } = MQ.SimpleCallback writer
  where
    writer :: MQ.MQTTClient -> MQ.Topic -> BL.ByteString -> [MQ.Property] -> IO ()
    writer _ t msg _ = do
      atomically $ do
        writeTQueue (runNodeQueue inQueue) (nodeId, parsed)
      where
        nodeId :: NodeT
        nodeId = (fromJust . fromStateTopic) t
        parsed :: EnergyState
        parsed = ((fromRight defaultES) . decodeMessage . toStrict) msg
        toStrict = BS.concat . BL.toChunks

queueStream :: (IsStream t, (Monad (t IO))) => SubQueue -> t IO (NodeT, EnergyState)
queueStream (NodeQueue q) = S.repeatM (atomically $ readTQueue q)

getThings :: Text.Text -> IO [Iot.ThingAttribute]
getThings thingTypeName = do
  let
    iiot = Iot.ioT{_svcPrefix="execute-api"} :: Service
    ttn = (Just thingTypeName) :: Maybe Text.Text
  lgr <- newLogger Trace stdout
  env <- newEnv Discover <&> set envLogger lgr . set envRegion Singapore <&> configure iiot
  runResourceT . runAWST env $ do
    things <- send (Iot.listThings & Iot.ltThingTypeName .~ ttn)
    return $ things ^. Iot.ltrsThings

thingName :: Iot.ThingAttribute -> Maybe ThingName
thingName t = t ^. Iot.taThingName

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
    n = Text.replace suffix "" $ Text.replace prefix "" t
    isValidTopic t' = prefix `Text.isPrefixOf` t' && suffix `Text.isSuffixOf` t'
    prefix = "/kibbutz/node/"
