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
                 KMState,
                 updateKM,
                 nodeStream,
                 monitorState,
                 runMonitor) where


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
import Control.Monad.State (StateT, put, MonadState, get, modify, runStateT)

import Data.Hashable (Hashable(..))
import qualified StmContainers.Map as SMap
import StateMonitor
import qualified Data.Time as Time

import Control.Monad.Reader

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
      atomically $ do
        writeTQueue (runNodeQueue inQueue) (nodeId, parsed)
      (writeBChan brickChan StateUpdate)
      where
        nodeId :: NodeT
        nodeId = (fromJust . fromStateTopic) t
        parsed :: EnergyState
        parsed = ((fromRight defaultES) . decodeMessage . toStrict) msg
        toStrict = BS.concat . BL.toChunks

queueStream :: (IsStream t) => SubQueue -> t IO (NodeT, EnergyState)
queueStream (NodeQueue q) = parallely $ S.repeatM $ (atomically $ readTQueue q) --

printQueueStream :: SerialT IO (NodeT, EnergyState) -> IO ()
printQueueStream = S.mapM_ print

nodeStream :: (IsStream t) => Time.UTCTime -> Kibbutz -> t IO NodeS
nodeStream initTime k = foldr (<>) (runNodeMonitor initTime n q) $ map (\n'-> runNodeMonitor initTime n' q) ns
  where n:ns = (nodes k)
        q = queueStream $ inQueue k


monitorState' = undefined
-- this should be a scan
-- tm = queueStream

monitorState :: (IsStream t, (Monad (StateT (KMState, KConnM) m))) => [NodeT] -> (t m (NodeT, NodeS)) -> StateT (KMState, KConnM) IO ([(NodeT, NodeS)], [(NodeT, Int)])
monitorState nodes ss = do
  (nodeStates, connStates) <- get
  let nS = (S.head ss) :: IO (Maybe (NodeT, NodeS))
      nS' :: [NodeT] -> IO [Maybe (NodeT, NodeS)] 
      nS' s = join $ map (\n -> filter (\(i, _)-> i == n) s)
  nsx'' <- mapM (nS' . nS) ss
  let
    ns :: [(NodeT, NodeS)]
    ns = map (fmap fromJust) $ filter (\a -> snd a /= Nothing) $ zip nodes nsx''
    ns'' (i, s) = do
      atomically $ do
        updateKM nodeStates i s
        c <- lookupKM connStates i
        let
          c' = fromMaybe 0 c
        updateKM connStates i (c'+1)
  _ <- mapM ns'' ns
  cns <- atomically $ readKM nodeStates nodes
  connCount <- atomically $ readKM connStates nodes
  return $ (cns, connCount)


runMonitor'' :: (Time.UTCTime -> Kibbutz -> KMState -> KConnM -> IO a) -> IO a 
runMonitor'' f = (\(f'', ns)-> join $ liftIO $ (atomically $ withKStates (ns) f'')) =<< (\f' -> (runReaderT (withKibbutz f') defKName)) =<< (liftIO $ withCurrentTime f)

runMonitor' :: a -> StateT (Kibbutz, KMState, KConnM, Serial a) IO ([(NodeT, NodeS)], [(NodeT, Int)])
runMonitor' a = do
  time <- liftIO $ Time.getCurrentTime
  (kibbutz, kmState, kconn, s) <- get
  (cns, connCount) <- liftIO $ monitorState time kibbutz kmState kconn
  put (kibbutz, kmState, kconn, s)
  return (cns, connCount)

runMonitor :: IO ([(NodeT, NodeS)], [(NodeT, Int)])
runMonitor = (runMonitor'' monitorState)

defKName = Text.pack "kibbutz-pilot-node"

withKStates :: [NodeT] -> (KMState -> KConnM -> a) -> STM a
withKStates ns f = do
  km <- initKMS ns
  kc <- initKMConn ns
  return $ f km kc

withCurrentTime :: (Time.UTCTime -> a) -> IO a
withCurrentTime f = do
  t <- Time.getCurrentTime
  return $ f t

type KibbutzName = Text.Text

withKibbutz :: (Kibbutz -> a) -> ReaderT KibbutzName IO (a, [NodeT])
withKibbutz f = do
  kibbutzName <- ask
  k <- liftIO $ getKibbutz kibbutzName
  return $ (f k, nodes k)


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
