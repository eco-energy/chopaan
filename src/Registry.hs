{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Registry (getThings, NodeId(..)) where


import qualified Data.Text as Text
import GHC.Generics (Generic)
import Data.Data
import Lens.Micro

-- AWS Imports
import qualified Network.AWS.IoT.ListThings as Iot
import qualified Network.AWS.IoT.Types as Iot
import Control.Monad.Trans.AWS
import Data.Maybe
import System.IO


type StateTopic = Text.Text

type ControlTopic = Text.Text

type ThingName = Text.Text

newtype NodeId = NodeId { unNodeId :: ThingName } deriving (Eq, Show, Ord, Data, Generic)

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


thingName :: Iot.ThingAttribute -> Maybe Text.Text
thingName t = t ^. Iot.taThingName


nameToTopics :: ThingName -> (StateTopic, ControlTopic)
nameToTopics name = (st, ct)
  where
    st :: StateTopic
    st = prefix <> n <> stChannel
    ct :: ControlTopic
    ct = prefix <> n <> cChannel
    stChannel = "/state"
    cChannel = "/control"
    prefix = "/kibbutz/node/"
    n = Text.replace ":" "" name

stateTopicToNodeId :: Text.Text -> NodeId
stateTopicToNodeId t = NodeId n
  where
    n = Text.replace "/state" "" $ Text.replace "/kibbutz/node" "" t
