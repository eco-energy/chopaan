{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Registry (getThings, NodeId(..), HasTopics(..), NodeT, ThingName, getKibbutz, Kibbutz(..), mkCallback) where


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
import Data.ProtoLens (defMessage)


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

newtype NodeQueue = NodeQueue { runNodeQueue :: TQueue (NodeT, EnergyState) } deriving (Eq, Generic)

initNodeQ :: STM (NodeQueue)
initNodeQ = do
  n <- newTQueue
  return $ NodeQueue n

newtype KibbutzState a = KibbutzState { unKibbutzState :: Map.Map NodeT a } deriving (Eq, Generic)

type KibbutzStateT = KibbutzState (NodeQueue)

initKibbutzStateT :: Set.Set NodeT -> STM (KibbutzStateT)
initKibbutzStateT ns = do
  qs <- replicateM (length ns) initNodeQ
  return $ KibbutzState $ Map.fromList $ zip (Set.toList ns) qs

-- window all the scanl fns

{--
printAudit :: KibbutzState -> IO ()
printAudit ns = PPT.printTable $ unKibbutzState ns
--}


data Kibbutz = Kibbutz
  { kname :: Text.Text
  , nodes :: Set.Set NodeT
  , queue :: NodeQueue
  } deriving (Eq, Generic)

instance Show Kibbutz where
  show Kibbutz {..} = Text.unpack $ (kname <> " Kibbutz, " <> (Text.pack $ show $ length nodes) <> " nodes")

kbtz = Kibbutz

getKibbutz :: Text.Text -> IO Kibbutz
getKibbutz n = do
  ts <- getThings n
  let
    ns = Set.fromList $ map (mkNode. fromJust . thingName) ts
  q <- atomically $ initNodeQ
  return $ kbtz n ns q

mkCallback :: Kibbutz -> MQ.MessageCallback
mkCallback Kibbutz { kname, nodes, queue } = MQ.SimpleCallback writer
  where
    writer :: MQ.MQTTClient -> MQ.Topic -> BL.ByteString -> [MQ.Property] -> IO ()
    writer _ t msg _ = do
      atomically $ do
        writeTQueue (runNodeQueue queue) (nodeId, parsed)
      where
        nodeId :: NodeT
        nodeId = (fromJust . fromStateTopic) t
        parsed :: EnergyState
        parsed = ((fromRight defaultES) . decodeMessage . toStrict) msg
        toStrict = BS.concat . BL.toChunks
        
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


defaultES :: EnergyState
defaultES = defMessage
               & batteryVoltage .~ 0
               & gridVoltage .~ 0
               & batteryToLoadCurrent .~ 0
               & batteryToGridCurrent .~ 0
               & gridToBatteryCurrent .~ 0
               & solarInputCurrent .~ 0
               & dutyCycle .~ 0
