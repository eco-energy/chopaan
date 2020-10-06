{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
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
                , Kibbutz(..), KibbutzEvents(..), getKibbutz, mkCallback, subStream
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
import qualified Proto.NodeMessages_Fields as NM
import Data.ProtoLens.Labels()


-- STM
import Control.Concurrent.STM

-- Streamly
import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Data.Unfold as UF
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Unfold.Types as UF
import qualified Streamly.Internal.Data.Stream.StreamD.Type as STy


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

type PubQueue = NodeQueue NodeT MeshFrame

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
  atomically $ writeTBQueue (runNodeQueue p) (n, toMeshFrame et)

toMeshFrame :: EnergyTransactionRequest -> MeshFrame
toMeshFrame etr = defMessage & #transaction .~ etr

type SensorSub = Subscriber NodeT EnergyState

type SensorSM = StreamMap NodeT EnergyState


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
    step _ = liftIO $ wrap <$> (atomically . readTBQueue . runNodeQueue $ sq)
      where
        wrap :: (NodeT, EnergyState) -> Maybe ((NodeT, EnergyState), ())
        wrap = Just . (, ())
          
mkCallback :: Kibbutz -> MQ.MessageCallback
mkCallback Kibbutz { inQueue, msgCount }  = MQ.SimpleCallback $ writer
  where
    writer :: MQ.MQTTClient -> MQ.Topic -> BL.ByteString -> [MQ.Property] -> IO ()
    writer _ t msg _ = do
      --print parsed
      atomically $ do
        case fromMeshFrame parsed of
          Nothing -> return ()
          Just es -> writeTBQueue (runNodeQueue inQueue) (nodeId, es)
        modifyTVar' msgCount (\a -> a + 1)
      where
        nodeId :: NodeT
        nodeId = (fromJust . fromStateTopic) t
        parsed :: MeshFrame
        parsed = ((fromRight . toMeshFrame' $ zeroMsg) . decodeMessage . toStrict) msg
        toStrict = BS.concat . BL.toChunks
        fromMeshFrame :: MeshFrame -> Maybe EnergyState
        fromMeshFrame m = m ^? NM.maybe'payload . _Just . _MeshFrame'State
        toMeshFrame' :: EnergyState -> MeshFrame
        toMeshFrame' etr = defMessage & #state .~ etr



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
    req = (Iot.listThings & Iot.ltThingTypeName .~ (Just thingTypeName))
  lgr <- newLogger Trace stdout
  env <- newEnv Discover <&> set envLogger lgr . set envRegion Singapore <&> configure iiot --  
  runResourceT . runAWST env $ do
    things <- S.toList
      $ S.map (\x -> x ^. Iot.ltrsThings)
      $ S.unfold pageUF req
    return $ concat things

pageUF :: forall m a r. (AWSPager a, AWSConstraint r m) => UF.Unfold m a (Rs a)
pageUF = UF.Unfold step inject
  where
    step :: Maybe a -> m (STy.Step (Maybe a) (Rs a)) 
    step (Just req) = do
      y <- send req
      return $ STy.Yield y (page req y)
    step Nothing = do
      return $ STy.Stop
    inject :: a -> m (Maybe a)
    inject = pure . Just

{-----------------------------------------------------------------------------------------

              Streamly APIs
-----------------------------------------------------------------------------------------}


--pageUF :: forall m a. (AWSPager a, AWSConstraint Env m, MonadBaseControl IO m) => a -> SerialT (AWST m) ((Rs a))




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



{-----------------------------------------------------------------------------

                 Monitor Tings
------------------------------------------------------------------------------}

type KMState = KibbutzMonitor NodeT NodeS

initKMS :: [NodeT] -> STM (KMState)
initKMS ns = initKM ns defNodeS

type KMSensor = KibbutzMonitor NodeT EnergyState


type KConnM = KibbutzMonitor NodeT Int

initKMConn :: [NodeT] -> STM KConnM
initKMConn ns = initKM ns 0
