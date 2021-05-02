{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances, DeriveAnyClass, DerivingStrategies, StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings, NamedFieldPuns, ScopedTypeVariables, TypeApplications, FlexibleContexts, TypeOperators, GADTs #-}
{-# LANGUAGE FlexibleInstances, TypeFamilies, InstanceSigs
, ConstraintKinds, ScopedTypeVariables, QuantifiedConstraints
, RankNTypes, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving, GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia, DeriveFunctor, DeriveFoldable, DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase, TypeOperators, TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Chopaan.Graph (module Chopaan.Graph, module AG, module NG) where

import Prelude hiding ((.), id)

import Shpadoinkle.Html as H
import ConCat.Misc (R, inNew, inNew2, (:*), (:+))
import GHC.Generics (Generic, Generic1)
import qualified Control.Newtype.Generics as N
import Control.DeepSeq (NFData)

import Control.Category
import Data.Aeson
import Data.Typeable
import Data.Function ((&))
import Data.Bifunctor
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import Data.Text as T hiding (zip)
import Data.Greskell.Graph (AVertex, AEdge, ElementData, Element, Vertex, Edge)
import Data.Greskell.GraphSON (FromGraphSON)
import Data.Greskell.Greskell (toGremlin)
import Data.Greskell.Binder
import Data.Greskell.GTraversal
  ( GTraversal, Walk, Transform, SideEffect, Filter, WalkType, gAddV, gAddE, gOut, gOutE, gId, gIn, gInE, gHasLabel, gProperty,
    source, sV, sV', gV, (&.), unsafeCastStart, unsafeCastEnd, (<*.>), sAddV, gHas2, liftWalk, gFrom, gTo, gSideEffect, ToGTraversal, AddAnchor )

import NetSpider.Graph  as NG (NodeAttributes(..), LinkAttributes(..), VFoundNode(..))
import NetSpider.Snapshot
import NetSpider.Timestamp (fromUTCTime, Timestamp(..))
import qualified Algebra.Graph.Labelled as AG

import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz
import Chopaan.Node.NodeId
import Chopaan.Kibbutz.Mesh
import Chopaan.Node.Folds (SensorS)
import Chopaan.Kibbutz.Transactor (Stake, TransactionStatus)
import Chopaan.Kibbutz (stakeConfig, meshConfig, statusConfig, getGridRoot)
import Chopaan.Node.HW

import Shpadoinkle.Widgets.Types

newtype M = M R

type N = (VKbtz :+ VHH :+ VHW :+ VPerson)

type E = (EKbtzIncludes :+ M :+ Watts :+ WattHours)

data KbtzGraph where
  Nod :: N -> KbtzGraph
  Connect :: E -> KbtzGraph

deriving instance Generic Timestamp
deriving instance NFData Timestamp
deriving instance (NFData n, NFData a) => NFData (SnapshotNode n a)
deriving instance (NFData n, NFData e) => NFData (SnapshotLink n e)


data SG where
  MeshSnapshot :: SnapshotGraph NodeMAC MeshNode RxSignal -> SG
  StakeSnapshot :: SnapshotGraph NodeMAC SensorS Stake -> SG
  StatusSnapshot :: SnapshotGraph NodeMAC SensorS TransactionStatus -> SG
  deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)


data GraphType = Mesh | Plan | Status
  deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, ToJSON, FromJSON, NFData, Humanize)


-- $ Constraints for edge labels and nodes
type GrConn f s = (Bounded s, Show s, Ord s, Eq s, Enum s, Show f, Monoid f, Ord f)

-- $ Constraints for edge labels and nodes, along with monad constraints
type GrConnM m f s = (Monad m, GrConn f s)

deriving instance Generic1 (AG.Graph flow)

newtype Gr flow state = Gr { unGr :: (AG.Graph flow state) }
  deriving stock (Eq, Ord, Show, Generic, Generic1)
  deriving newtype (Num, Functor, Bifunctor)


emptyGr :: (GrConn flow state) => Gr flow state
emptyGr = Gr AG.empty

grProps :: [(Text, Prop m (Gr flow state))]
grProps = []

grEdge :: (Show a, Show b, Show c) => (a, b, c) -> Text
grEdge (l, e, e') = (pack . show $ l)

instance N.Newtype (Gr flow state)

-- $ Shpadoinkle Instances
instance (Show state, Show flow) => Humanize (Gr flow state)

fromSnapshot :: forall n l v. (Monoid l, Ord n) => SnapshotGraph n v l -> Gr l v
fromSnapshot (nodes, links) = Gr . AG.edges $ castLink <$> links
  where
    castLink l = (linkAttributes l, sourceAttrs l, destAttrs l)
    nmap = Map.fromList $ zip (nodeId <$> nodes) (nodeAttributes <$> nodes)
    sourceAttrs l = fromJust $ nmap Map.! (sourceNode l)
    destAttrs l = fromJust $ nmap Map.! (destinationNode l) 
    
newtype GrNode = GrNode Int
  deriving (Eq, Ord, Typeable, Show)
  deriving newtype (Num)


newtype VKbtz = VKbtz AVertex
  deriving (Eq, Show)
  deriving newtype (FromGraphSON, ElementData, Element, Vertex)

newtype VHH = VHH AVertex
  deriving (Eq, Show)
  deriving newtype (FromGraphSON, ElementData, Element, Vertex)

newtype VHW = VHW AVertex
  deriving (Eq, Show)
  deriving newtype (FromGraphSON, ElementData, Element, Vertex)

newtype VPerson = VPerson AVertex
  deriving (Eq, Show)
  deriving newtype (FromGraphSON, ElementData, Element, Vertex)

newtype EKbtzIncludes = EKbtzIncludes AEdge
  deriving (Eq, Show)
  deriving newtype (FromGraphSON, ElementData, Element, Edge)


newtype EHasHW = EHasHW AEdge
  deriving (Eq, Show)
  deriving newtype (FromGraphSON, ElementData, Element, Edge)

newtype EOwnsHW = EOwnsHW AEdge
  deriving (Eq, Show)
  deriving newtype (FromGraphSON, ElementData, Element, Edge)

gOutKbtzIncludes :: Walk Transform VKbtz VHH
gOutKbtzIncludes = gOut ["kbtzIncludes"]

gInKbtzIncludes :: Walk Transform VHH VKbtz
gInKbtzIncludes = gIn ["kbtzIncludes"]

gOutHasHW :: Walk Transform VHH VHW
gOutHasHW = gOut ["hasHW"]

gInOwnsHW :: Walk Transform VHW VPerson
gInOwnsHW = gIn ["ownsHW"]


data AKbtz = AKbtz
  { akId :: KbtzName
  , hasNodes :: [ANode]
  } deriving (Eq, Ord, Show)

data ANode = ANode
  { anId :: NodeMAC
  } deriving (Eq, Ord, Show)


addKbtz :: KbtzName -> Binder (GTraversal SideEffect () VKbtz)
addKbtz k = do
  kid <- newBind k
  let
    addId :: Binder (Walk SideEffect VKbtz VKbtz)
    addId = return $ gProperty "@kbtz_id" kid
  addId <*.> (pure $ sAddV "kbtz" $ source "g")

addNode :: KbtzName -> ANode -> Binder (GTraversal SideEffect () EKbtzIncludes)
addNode k ANode{anId} = do
  n <- newBind anId
  kb <- gGetKbtzByKbtzId k
  let
    addId :: Walk SideEffect VHH VHH
    addId = gProperty "@knode_id" n
    addV :: Walk SideEffect VKbtz VHH
    addV = gAddV "knode"
    withNode :: Walk SideEffect VKbtz VHH
    withNode = (addId . addV)
    no :: Walk SideEffect VKbtz VKbtz
    no = (gSideEffect withNode) . (liftWalk kb)
  x <- gKbtzNodes k
  x' <- gHasNodeId anId
  let
    thisN :: Walk Transform VKbtz VHH
    thisN = x' . x . (liftWalk kb)
    anc :: AddAnchor VKbtz VHH
    anc = gTo thisN
    ac = gFrom (liftWalk kb)
    ed :: Walk SideEffect VKbtz EKbtzIncludes
    ed = gAddE "kbtzIncludes" anc
  return $ (liftWalk allKbtz) &. (liftWalk no) &. ed


emitsAEdge :: ToGTraversal g => g c s AEdge -> g c s AEdge
emitsAEdge = id

writeHWConfig :: HW Double -> Binder (Walk SideEffect VHW VHW)
writeHWConfig hw = (unsafeCastStart . unsafeCastEnd) <$> (writeNodeAttributes hw)

addHWConfig :: KbtzName -> NodeMAC -> (HW Double) -> Binder (GTraversal SideEffect () VHW)
addHWConfig k n s = (writeHWConfig s)
  <*.> (pure $ sAddV "hwConfig" $ source "g")



--addNodeToKbtz :: SensorS -> HW Double -> NodeMAC -> 

allV :: GTraversal Transform () AVertex
allV = source "g" & sV []

allKbtz :: GTraversal Transform () VKbtz
allKbtz =  source "g" & sV [] &. gHasLabel "kbtz"

allNodes :: GTraversal Transform () VHH
allNodes = source "g" & sV [] &. gHasLabel "knode"

gHasKbtzId :: (WalkType c) => KbtzName -> Binder (Walk c VKbtz VKbtz)
gHasKbtzId (KbtzId k) = do
  kid <- newBind k
  return $ gHas2 "@kbtz_id" kid

gHasNodeId :: (WalkType c) => NodeMAC -> Binder (Walk c VHH VHH)
gHasNodeId n = do
  nid <- newBind n
  return $ gHas2 "@knode_id" nid

gGetKbtzByKbtzId :: KbtzName -> Binder (Walk Filter VKbtz VKbtz)
gGetKbtzByKbtzId kid = gHasKbtzId kid

gKbtzNodes :: KbtzName -> Binder (Walk Transform VKbtz VHH)
gKbtzNodes k = do
  kb <- (gGetKbtzByKbtzId k)
  return (gOutKbtzIncludes <<< liftWalk kb)
