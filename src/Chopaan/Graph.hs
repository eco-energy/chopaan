{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances, DeriveAnyClass, DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings, NamedFieldPuns, ScopedTypeVariables, TypeApplications, FlexibleContexts #-}
module Chopaan.Graph where

import Prelude hiding ((.), id)
import Control.Category
import Data.Function ((&))

import Data.Greskell.Graph (AVertex, AEdge, ElementData, Element, Vertex, Edge)
import Data.Greskell.GraphSON (FromGraphSON)
import Data.Greskell.Greskell (toGremlin)
import Data.Greskell.Binder
import Data.Greskell.GTraversal
  ( GTraversal, Walk, Transform, SideEffect, Filter, WalkType, gAddV, gAddE, gOut, gOutE, gId, gIn, gInE, gHasLabel, gProperty,
    source, sV, sV', gV, (&.), unsafeCastStart, unsafeCastEnd, (<*.>), sAddV, gHas2, liftWalk, gFrom, gTo, gSideEffect, ToGTraversal, AddAnchor )

import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..), VFoundNode(..))

import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz
import Chopaan.Node.NodeId
import Chopaan.Node.HW

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
