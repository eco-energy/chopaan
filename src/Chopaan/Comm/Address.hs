{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

module Chopaan.Comm.Address where

import Chopaan.Node.NodeId
import Data.Hashable
import qualified Network.MQTT.Topic as MQ
import Data.Text (Text)
import Chopaan.Kibbutz.AWS.Things

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
