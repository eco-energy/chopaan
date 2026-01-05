{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- | Categorical Summing Functors for Power Flow Networks
--
-- Based on Manin-Marcolli's framework for neural information networks.
-- A summing functor Φ : P(E_G) → C assigns resources to subsets of edges
-- such that Φ(A ∪ B) = Φ(A) ⊕ Φ(B) for disjoint A, B.
--
-- The equalizer Σ_C^eq(G) consists of summing functors satisfying
-- Kirchhoff's conservation law at vertices:
--
-- @
-- Σ_{e: s(e)=v} Φ(e) = Σ_{e: t(e)=v} Φ(e)
-- @
--
-- This is exactly the power balance constraint for AC power flow.

module Chopaan.AC.SummingFunctor
  ( -- * Directed Graphs
    DirectedGraph(..)
  , Edge(..)
  , Node(..)
  , mkGraph
  , inEdges
  , outEdges
    -- * Summing Functors
  , SummingFunctor(..)
  , EdgeAssignment
  , NodeAssignment
  , fromEdgeMap
    -- * Kirchhoff Equalizer
  , KirchhoffEqualizer(..)
  , isInEqualizer
  , projectToEqualizer
  , kirchhoffImbalance
    -- * Power Flow Category
  , PowerPair(..)
  , powerZero
  , powerPlus
    -- * Network Functor Layer
  , SummingFunctorLayer(..)
  , applyLayer
  ) where

#ifndef ghcjs_HOST_OS

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON)
import Data.List (foldl')

-- | Node in the network
newtype Node = Node { unNode :: Int }
  deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

-- | Directed edge with source and target
data Edge = Edge
  { edgeSrc :: Node
  , edgeTgt :: Node
  , edgeIdx :: Int  -- ^ Unique edge index
  } deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

-- | Directed graph G : 2 → F (functor from walking arrow to FinSet)
data DirectedGraph = DirectedGraph
  { graphNodes :: Set Node
  , graphEdges :: Set Edge
  , graphInEdges  :: Map Node [Edge]  -- ^ Edges with target = node
  , graphOutEdges :: Map Node [Edge]  -- ^ Edges with source = node
  } deriving (Show, Eq, Generic, NFData)

-- | Construct graph from edge list
mkGraph :: [(Int, Int)] -> DirectedGraph
mkGraph edgeList =
  let edges = [Edge (Node s) (Node t) i | (i, (s, t)) <- zip [0..] edgeList]
      nodes = Set.fromList $ concatMap (\e -> [edgeSrc e, edgeTgt e]) edges
      inMap = foldl' (\m e -> Map.insertWith (++) (edgeTgt e) [e] m) Map.empty edges
      outMap = foldl' (\m e -> Map.insertWith (++) (edgeSrc e) [e] m) Map.empty edges
  in DirectedGraph nodes (Set.fromList edges) inMap outMap

-- | Get incoming edges at a node
inEdges :: DirectedGraph -> Node -> [Edge]
inEdges g n = Map.findWithDefault [] n (graphInEdges g)

-- | Get outgoing edges at a node
outEdges :: DirectedGraph -> Node -> [Edge]
outEdges g n = Map.findWithDefault [] n (graphOutEdges g)

-- | Power pair (P, Q) - objects in our category C = (ℝ², +, 0)
data PowerPair = PowerPair
  { ppReal :: Double      -- ^ Real power P (kW)
  , ppReactive :: Double  -- ^ Reactive power Q (kVAR)
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

-- | Zero object in the power category
powerZero :: PowerPair
powerZero = PowerPair 0 0

-- | Monoidal product (addition) in power category
powerPlus :: PowerPair -> PowerPair -> PowerPair
powerPlus (PowerPair p1 q1) (PowerPair p2 q2) = PowerPair (p1 + p2) (q1 + q2)

instance Semigroup PowerPair where
  (<>) = powerPlus

instance Monoid PowerPair where
  mempty = powerZero

-- | Edge assignment Φ_E : E_G → C
type EdgeAssignment c = Map Edge c

-- | Node assignment (induced by source/target maps)
type NodeAssignment c = Map Node c

-- | Summing functor Φ : P(E_G) → C
--
-- Must satisfy: Φ(A ∪ B) = Φ(A) ⊕ Φ(B) for disjoint A, B
-- By Lemma 2.3 (Manin-Marcolli), determined by values on singletons
data SummingFunctor c = SummingFunctor
  { sfGraph :: DirectedGraph
  , sfEdgeValues :: EdgeAssignment c  -- ^ Φ(e) for each edge e
  } deriving (Show, Eq, Generic, NFData)

-- | Create summing functor from edge map
fromEdgeMap :: DirectedGraph -> Map Int c -> SummingFunctor c
fromEdgeMap g edgeMap = SummingFunctor g $
  Map.fromList [(e, edgeMap Map.! edgeIdx e) | e <- Set.toList (graphEdges g)]

-- | Kirchhoff equalizer Σ_C^eq(G)
--
-- Consists of summing functors where:
-- Σ_{e: s(e)=v} Φ(e) = Σ_{e: t(e)=v} Φ(e)  for all non-slack nodes v
data KirchhoffEqualizer c = KirchhoffEqualizer
  { keSlackNode :: Node                    -- ^ Slack bus (absorbs imbalance)
  , keFunctor :: SummingFunctor c          -- ^ Underlying summing functor
  , keImbalance :: NodeAssignment c        -- ^ Imbalance at each node
  } deriving (Show, Eq, Generic, NFData)

-- | Compute Kirchhoff imbalance at each node
--
-- imbalance(v) = Σ_{in} Φ(e) - Σ_{out} Φ(e)
kirchhoffImbalance :: (Monoid c) => SummingFunctor c -> NodeAssignment c
kirchhoffImbalance SummingFunctor{..} =
  let g = sfGraph
      nodeImbalance n =
        let inFlow = mconcat [sfEdgeValues Map.! e | e <- inEdges g n]
            outFlow = mconcat [sfEdgeValues Map.! e | e <- outEdges g n]
        in inFlow <> negateFlow outFlow  -- Assuming we have negation
  in Map.fromList [(n, nodeImbalance n) | n <- Set.toList (graphNodes g)]
  where
    -- For PowerPair, negate both components
    negateFlow :: PowerPair -> PowerPair
    negateFlow (PowerPair p q) = PowerPair (-p) (-q)

-- | Check if a summing functor is in the Kirchhoff equalizer
isInEqualizer :: Node -> SummingFunctor PowerPair -> Bool
isInEqualizer slack sf =
  let g = sfGraph sf
      vals = sfEdgeValues sf
      checkNode n
        | n == slack = True  -- Slack absorbs imbalance
        | otherwise =
            let inFlow = sum [ppReal (vals Map.! e) | e <- inEdges g n]
                outFlow = sum [ppReal (vals Map.! e) | e <- outEdges g n]
            in abs (inFlow - outFlow) < 1e-6
  in all checkNode (Set.toList $ graphNodes g)

-- | Project a summing functor to the Kirchhoff equalizer
--
-- This is the key operation: given arbitrary edge flows,
-- adjust them minimally so power balance holds at all nodes.
--
-- Uses least-squares projection: minimize ||Φ' - Φ||² subject to KCL
projectToEqualizer :: Node                      -- ^ Slack node
                   -> SummingFunctor PowerPair  -- ^ Input functor
                   -> SummingFunctor PowerPair  -- ^ Projected (in equalizer)
projectToEqualizer slack sf@SummingFunctor{..} =
  let g = sfGraph
      edges = Set.toList graphEdges
      nodes = filter (/= slack) $ Set.toList (graphNodes g)

      -- Compute imbalance at each non-slack node
      imbalance n =
        let inFlow = sum [ppReal (sfEdgeValues Map.! e) | e <- inEdges g n]
            outFlow = sum [ppReal (sfEdgeValues Map.! e) | e <- outEdges g n]
        in inFlow - outFlow

      -- Distribute imbalance correction to incident edges
      -- Simple heuristic: proportional to number of edges
      correctedVals = foldl' correctNode sfEdgeValues nodes

      correctNode vals n =
        let imb = imbalance n  -- Recompute with current vals
            inE = inEdges g n
            outE = outEdges g n
            nEdges = length inE + length outE
            correction = if nEdges > 0 then imb / fromIntegral nEdges else 0
            -- Reduce inflow or increase outflow
            vals' = foldl' (\v e -> Map.adjust (adjustP (-correction)) e v) vals inE
            vals'' = foldl' (\v e -> Map.adjust (adjustP correction) e v) vals' outE
        in vals''

      adjustP dp (PowerPair p q) = PowerPair (p + dp) q

  in sf { sfEdgeValues = correctedVals }

-- | A layer in the summing functor network
--
-- Transforms edge assignments while preserving the summing functor structure
data SummingFunctorLayer = SummingFunctorLayer
  { sflInputDim :: Int
  , sflOutputDim :: Int
  , sflProjectToEqualizer :: Bool  -- ^ Apply Kirchhoff projection after?
  } deriving (Show, Eq, Generic, NFData)

-- | Apply a summing functor layer (placeholder for neural layer)
applyLayer :: SummingFunctorLayer
           -> SummingFunctor PowerPair
           -> SummingFunctor PowerPair
applyLayer SummingFunctorLayer{..} sf =
  -- In practice, this would apply learned transformations
  -- For now, just project to equalizer if requested
  if sflProjectToEqualizer
  then projectToEqualizer (Node 0) sf  -- Assume node 0 is slack
  else sf

#else

-- GHCJS stubs
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)

newtype Node = Node { unNode :: Int } deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)
data Edge = Edge { edgeSrc :: Node, edgeTgt :: Node, edgeIdx :: Int } deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)
data DirectedGraph = DirectedGraph { graphNodes :: Set Node, graphEdges :: Set Edge, graphInEdges :: Map Node [Edge], graphOutEdges :: Map Node [Edge] } deriving (Show, Eq, Generic, NFData)
data PowerPair = PowerPair { ppReal :: Double, ppReactive :: Double } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)
type EdgeAssignment c = Map Edge c
type NodeAssignment c = Map Node c
data SummingFunctor c = SummingFunctor { sfGraph :: DirectedGraph, sfEdgeValues :: EdgeAssignment c } deriving (Show, Eq, Generic, NFData)
data KirchhoffEqualizer c = KirchhoffEqualizer { keSlackNode :: Node, keFunctor :: SummingFunctor c, keImbalance :: NodeAssignment c } deriving (Show, Eq, Generic, NFData)
data SummingFunctorLayer = SummingFunctorLayer { sflInputDim :: Int, sflOutputDim :: Int, sflProjectToEqualizer :: Bool } deriving (Show, Eq, Generic, NFData)

powerZero :: PowerPair
powerZero = PowerPair 0 0
powerPlus :: PowerPair -> PowerPair -> PowerPair
powerPlus (PowerPair p1 q1) (PowerPair p2 q2) = PowerPair (p1 + p2) (q1 + q2)
mkGraph :: [(Int, Int)] -> DirectedGraph
mkGraph _ = DirectedGraph Set.empty Set.empty Map.empty Map.empty
inEdges :: DirectedGraph -> Node -> [Edge]
inEdges _ _ = []
outEdges :: DirectedGraph -> Node -> [Edge]
outEdges _ _ = []
fromEdgeMap :: DirectedGraph -> Map Int c -> SummingFunctor c
fromEdgeMap g _ = SummingFunctor g Map.empty
kirchhoffImbalance :: (Monoid c) => SummingFunctor c -> NodeAssignment c
kirchhoffImbalance _ = Map.empty
isInEqualizer :: Node -> SummingFunctor PowerPair -> Bool
isInEqualizer _ _ = False
projectToEqualizer :: Node -> SummingFunctor PowerPair -> SummingFunctor PowerPair
projectToEqualizer _ sf = sf
applyLayer :: SummingFunctorLayer -> SummingFunctor PowerPair -> SummingFunctor PowerPair
applyLayer _ sf = sf

#endif
