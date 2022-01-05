{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
module Chopaan.Graph.VI where

import ConCat.Misc
import Chopaan.Node.NodeSensors
import qualified Chopaan.Graph.Algebraic as AG


-- $ vertices are potentials
-- $ edges are flows
-- $ s is the field

type ViG s = AG.Graph (I s) (V s)

type ViGk k s = AG.Graph (I s) (k :* V s)

vertex :: forall s. Num s => s -> ViG s 
vertex = AG.vertex . v

edge :: forall s. Num s => s -> V s -> V s -> ViG s 
edge di dv dv' = AG.connect (i di) (AG.vertex dv) (AG.vertex dv')

vertexK :: forall s k. Num s => k :* s -> ViGk k s
vertexK = AG.vertex . fmap v

edgeK :: forall s k. Num s => s -> (k :* s) -> (k :* s) -> ViGk k s
edgeK di dv dv' = AG.connect (i di) (vertexK dv) (vertexK dv')



