{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, UndecidableInstances, DeriveGeneric, DeriveAnyClass, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, StandaloneDeriving #-}
module Chopaan.Graph.Kbtz.Schema where

import GHC.Generics

import Control.DeepSeq

-- import Codec.Winery
import Data.Aeson (ToJSON, FromJSON)
import Data.Time (UTCTime)
import qualified Data.Text as T
import Data.Greskell.Graph (AVertex, AEdge, ElementData(..), Element(..), Vertex, Edge, ElementData(..))
import Data.Greskell.PMap (pMapToFail)
import Data.Greskell.GraphSON (FromGraphSON(..))


import Chopaan.Node.NodeId
import Chopaan.Kibbutz.KbtzId
import Chopaan.Node.HW
import Chopaan.Graph.Kbtz.Types

newtype VKbtz = VKbtz AVertex
  deriving (Eq, Show)
  deriving newtype (FromGraphSON, ElementData, Element, Vertex)

newtype VHH = VHH AVertex
  deriving (Eq, Show)
  deriving newtype (FromGraphSON, ElementData, Element, Vertex)

newtype VHW = VHW AVertex
  deriving (Eq, Show)
  deriving newtype (FromGraphSON, ElementData, Element, Vertex)

newtype VLastSync = VLastSync AVertex
  deriving (Eq, Show)
  deriving newtype (FromGraphSON, ElementData, Element, Vertex)

newtype EHHHasHW = EHasHW AEdge
  deriving (Eq, Show)
  deriving newtype (FromGraphSON, ElementData, Element, Edge)

newtype HHLastSynced = HHLastSynced AEdge
  deriving (Eq, Show)
  deriving newtype (FromGraphSON, ElementData, Element, Edge)


-- A Kbtz is not a node, it's a graph where the vertices are households
-- and the edges are dunno. But AKbtz is a hypergraph node.
data AKbtz = AKbtz
  { akId :: KbtzName
  , akLocation :: T.Text
  , createdOn :: UTCTime
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, NFData)

instance HasV VKbtz AKbtz where
  vProps = encode'

instance HasV VHH ANode where
  vProps = encode'

instance HasV VHW (HW Double) where
  vProps = encode'

instance FromGraphSON AKbtz where
  parseGraphSON gv = (pMapToFail . parse') =<< parseGraphSON gv


data ANode = ANode
  { anId :: NodeMAC
  , anName :: T.Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, NFData)


instance FromGraphSON ANode where
  parseGraphSON gv = (pMapToFail . parse') =<< parseGraphSON gv
