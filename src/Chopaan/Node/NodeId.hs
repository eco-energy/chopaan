{-# LANGUAGE DeriveGeneric #-}
module Chopaan.Node.NodeId where

import Data.Hashable (Hashable(..))
import Data.Csv (ToField(..))
import GHC.Generics
import qualified Data.Text as Text

newtype NodeId a = NodeId { unNodeId :: a } deriving (Eq, Ord, Generic)

instance Show a => Show (NodeId a) where
  show a = "NodeId :: " <> (show a) 

instance (Hashable a) => Hashable (NodeId a) where
  hashWithSalt n (NodeId a) = hashWithSalt n a

instance (ToField a) => ToField (NodeId a) where
  toField (NodeId a) = toField a 

type ThingName = Text.Text


type NodeMAC = NodeId ThingName
