{-# LANGUAGE GeneralizedNewtypeDeriving, UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Chopaan.Graph where

import Prelude hiding ((.), id)
import Control.Category
import Data.Function ((&))

import Data.Greskell.Graph (AVertex, AEdge, ElementData, Element, Vertex, Edge)
import Data.Greskell.GraphSON (FromGraphSON)
import Data.Greskell.Greskell (toGremlin)
import Data.Greskell.Binder
import Data.Greskell.GTraversal
  ( GTraversal, Walk, Transform, SideEffect, Filter, WalkType, gAddV, gOut, gOutE, gIn, gHasLabel, gProperty,
    source, sV, gV, (&.), unsafeCastStart, unsafeCastEnd, (<*.>), sAddV, gHas2 )

import NetSpider.Graph (NodeAttributes(..), LinkAttributes(..), VFoundNode(..))

import Chopaan.Kibbutz.KbtzId
import Chopaan.Kibbutz
import Chopaan.Node.NodeId
import Chopaan.Node.HW

newtype VKbtz = VKbtz AVertex
  deriving (Eq, Show, FromGraphSON, ElementData, Element, Vertex)

newtype VHH = VHH AVertex
  deriving (Eq, Show, FromGraphSON, ElementData, Element, Vertex)

newtype VHW = VHW AVertex
  deriving (Eq, Show, FromGraphSON, ElementData, Element, Vertex)

newtype VPerson = VPerson AVertex
  deriving (Eq, Show, FromGraphSON, ElementData, Element, Vertex)

newtype EKbtzIncludes = EKbtzIncludes AEdge
  deriving (Eq, Show, FromGraphSON, ElementData, Element, Edge)


newtype EHasHW = EHasHW AEdge
  deriving (Eq, Show, FromGraphSON, ElementData, Element, Edge)

newtype EOwnsHW = EOwnsHW AEdge
  deriving (Eq, Show, FromGraphSON, ElementData, Element, Edge)

gOutKbtzIncludes :: Walk Transform VKbtz VHH
gOutKbtzIncludes = gOut ["kbtzIncludes"]

gOutHasHW :: Walk Transform VHH VHW
gOutHasHW = gOut ["hasHW"]

gInOwnsHW :: Walk Transform VHW VPerson
gInOwnsHW = gIn ["ownsHW"]


addKbtz :: KbtzName -> Binder (Walk SideEffect () VKbtz)
addKbtz k = do
  kid <- newBind k
  return $ gProperty "@kbtz_id" kid <<< gAddV "kbtz"


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

gHasKbtzId :: (WalkType c) => KbtzName -> Binder (Walk c VKbtz VKbtz)
gHasKbtzId (KbtzId k) = return . (gHas2 "@kbtz_id") =<< (newBind k)  

gGetKbtzByKbtzId :: KbtzName -> Binder (Walk Transform s VKbtz)
gGetKbtzByKbtzId kid = do
  f <- gHasKbtzId kid
  return (f <<< gV [])

gKbtzNodes :: KbtzName -> Binder (Walk Transform VKbtz VHH)
gKbtzNodes k = do
  kb <- (gGetKbtzByKbtzId k)
  return (gOutKbtzIncludes <<< kb)
