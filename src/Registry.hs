{-# LANGUAGE TupleSections #-}
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
{-# LANGUAGE ScopedTypeVariables #-}
module Registry (NodeT, HasTopics(..), ThingName
                , Kibbutz(..), KibbutzEvents(..), getKibbutz, mkCallback, defKName, subStream
                , PubQueue, writeToPubQ
                , NodeQueue(..), initNodeQueue, writeToNodeQueue
                , KConnM, KMState, KMSensor, initKMS, initKMConn
                , SensorSM, SensorSub, duplicateS, Message(..)
                ) where


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
import Data.ProtoLens (Message(..))


import StateMonitor
import Subscriber (Subscriber, StreamMap)
import Control.Monad.Reader

import qualified Control.Concurrent.STM.TChan as TChan


mkNode :: ThingName -> NodeT
mkNode = NodeId

newtype NodeQueue a b = NodeQueue { runNodeQueue :: ((HasTopics a, Message b) => TBQueue (a, b)) }

type PubQueue = NodeQueue NodeT EnergyTransactionRequest

type SubQueue = NodeQueue NodeT EnergyState

initNodeQueue :: STM (NodeQueue a b)
initNodeQueue = do
  n <- newTBQueue 10000
  return $ NodeQueue n

writeToNodeQueue :: (HasTopics a, Message b) => NodeQueue a b -> a -> b -> IO ()
writeToNodeQueue q n m = do
  atomically $ writeTBQueue (runNodeQueue q) (n, m)

writeToPubQ :: PubQueue -> NodeT -> EnergyTransactionRequest -> IO ()
writeToPubQ p n et = do
  atomically $ writeTBQueue (runNodeQueue p) (n, et)

type SensorSub = Subscriber NodeT EnergyState

type SensorSM = StreamMap NodeT EnergyState

type MetricsSM = StreamMap NodeT NodeS

data Kibbutz = Kibbutz
  { kname :: Text.Text
  , nodes :: [NodeT]
  , inQueue :: SubQueue
  , outQueue :: PubQueue
  , msgCount :: TVar Int
  } deriving (Generic)


instance Show Kibbutz where
  show Kibbutz {..} = Text.unpack $ (kname <> " Kibbutz, " <> (Text.pack $ show $ length nodes) <> " nodes")

data KibbutzEvents = StateUpdate deriving (Eq, Ord, Show)

kbtz :: Text.Text -> [NodeT] -> SubQueue -> PubQueue -> TVar Int -> Kibbutz
kbtz = Kibbutz

getKibbutz :: Text.Text -> IO Kibbutz
getKibbutz n = do
  ts <- getThings n
  let
    ns = map (mkNode . fromJust . thingName) ts
  iq <- atomically $ initNodeQueue
  oq <- atomically $ initNodeQueue
  mc <- newTVarIO 0
  return $ kbtz n ns iq oq mc

subStream :: forall t m. (IsStream t, MonadAsync m) => SubQueue -> t m (NodeT, EnergyState)
subStream sq = asyncly $ S.unfoldrM step ()
  where
    step :: () -> m (Maybe ((NodeT, EnergyState), ()))
    step _ = liftIO $  (fmap (, ())) <$> (atomically . tryReadTBQueue . runNodeQueue $ sq)

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
        parsed = ((fromRight zeroMsg) . decodeMessage . toStrict) msg
        toStrict = BS.concat . BL.toChunks

defKName :: KibbutzName
defKName = Text.pack "kibbutz-pilot-node"

type KibbutzName = Text.Text


{--------------------------------------------------------------------------------------------------------

                   Thing Tings and Rules for Topics
---------------------------------------------------------------------------------------------------------}

type ThingName = Text.Text

type NodeT = NodeId ThingName

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

thingName :: Iot.ThingAttribute -> Maybe ThingName
thingName t = t ^. Iot.taThingName


iot :: BS.ByteString -> Service
iot svc = Iot.ioT{_svcPrefix=svc} :: Service

getThings :: Text.Text -> IO [Iot.ThingAttribute]
getThings thingTypeName = do
  let
    iiot = iot "execute-api"
    ttn = (Just thingTypeName) :: Maybe Text.Text
  lgr <- newLogger Trace stdout
  env <- newEnv Discover <&> set envLogger lgr . set envRegion Singapore <&> configure iiot --  
  runResourceT . runAWST env $ do
    things <- send (Iot.listThings & Iot.ltThingTypeName .~ ttn)
    return $ things ^. Iot.ltrsThings



--registerThing :: (exceptions-0.10.3:Control.Monad.Catch.MonadCatch m, unliftio-core-0.1.2.0:Control.Monad.IO.Unlift.MonadUnliftIO m) => p -> m (a, b)
registerThing t = do
  let
    iiot = iot "execute-api"
  lgr <- newLogger Trace stdout
  env <- newEnv Discover <&> set envLogger lgr . set envRegion Singapore <&> configure iiot
  runResourceT . runAWST env $ do
    (cert, reg) <- undefined (Iot.registrationConfig)
    return $ (cert, reg) 


{-----------------------------------------------------------------------------------------

              Streamly APIs
-----------------------------------------------------------------------------------------}

duplicateS
  :: forall t m a .
  MonadAsync m
  => IsStream t
  => Monad (t m)
  => t m a
  -> m (t m a, t m a)
duplicateS src = do
  (writeChan', readChan1, readChan2) <- liftIO $ do
    chan <- TChan.newBroadcastTChanIO
    chan' <- atomically $ TChan.dupTChan chan
    chan'' <- atomically $ TChan.dupTChan chan
    pure (chan, chan', chan'')
  let
    writes =
      S.mapM (liftIO . atomically . TChan.writeTChan writeChan') src
    reads1 =
      S.repeatM (liftIO $ atomically $ TChan.readTChan readChan1)
    reads2 =
      S.repeatM (liftIO $ atomically $ TChan.readTChan readChan2)
    tp :: (t m a, t m a)
    tp =
      (fmap (fromRight undefined) $ S.filter isRight $ (Left <$> writes) `async` (Right <$> reads1), reads2)
  pure $ tp



duplicateSN
  :: forall t m a .
  MonadAsync m
  => IsStream t
  => Monad (t m)
  => Show a
  => t m a
  -> Int
  -> m [t m a]
duplicateSN src n = do
  (writeChan', rCs) <- liftIO $ do
    chan <- TChan.newBroadcastTChanIO
    (rChans') <- replicateM n (atomically $ TChan.dupTChan chan)
    pure (chan, rChans')
  let
    (r:rChans) = rCs
    writes :: t m ()
    writes = (S.mapM (liftIO . atomically . TChan.writeTChan writeChan') $ src)
    reads :: TChan.TChan a -> t m a
    reads c = (fmap (fromJust undefined) $ S.filter (not . isNothing) $ S.repeatM (liftIO $ atomically $ TChan.tryReadTChan c))
    cs :: [t m a]
    cs = map reads rChans
    h = (fmap (fromRight undefined) $ S.filter isRight $ (Left <$> writes) `serial` (Right <$> (S.repeatM (liftIO $ atomically $ TChan.readTChan r))))
  pure $ (h:cs)


duplicateSN'
  :: MonadAsync m
  => IsStream t
  => Monad (t m)
  => t m a
  -> Int
  -> m [t m a]
duplicateSN' src n = fmap tupleToList (replicateM n (duplicateS src))
  where
    tupleToList :: [(a, a)] -> [a]
    tupleToList ((a,b):xs) = a : b : tupleToList xs
    tupleToList _          = []


{-----------------------------------------------------------------------------

                 Monitor Tings
------------------------------------------------------------------------------}

type KMState = KibbutzMonitor NodeT NodeS

initKMS :: [NodeT] -> STM (KMState)
initKMS ns = initKM ns defNodeS

type KMSensor = KibbutzMonitor NodeT EnergyState

initKSensorM :: [NodeT] -> STM (KMSensor)
initKSensorM ns = initKM ns zeroMsg

type KConnM = KibbutzMonitor NodeT Int

initKMConn :: [NodeT] -> STM KConnM
initKMConn ns = initKM ns 0
