{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

-- | Properad-Based Power Flow Networks
--
-- Implementation of Manin-Marcolli §2.3.2: Summing functors with properad
-- composition. By Corollary 2.20, a network summing functor Φ ∈ Σ^prop_C(G)
-- is completely determined by its values on corollas.
--
-- Key concepts:
--
-- * Corolla C(v): vertex v with all incident half-edges
-- * Properad composition: grafting corollas along shared edges
-- * P(m,n): category of "processes" with m inputs and n outputs
--
-- For power flow:
-- * Each corolla outputs flows on incident edges
-- * Local conservation enforced within each corolla
-- * Grafting identifies output flows with input flows on shared edges
--
-- Advantages over projection-based approach:
-- * O(n) instead of O(n³) - no matrix inverse
-- * Compositional - build from local modules
-- * Handles topology changes naturally
-- * Conservation is local, not global post-hoc projection

module Chopaan.AC.Properad
  ( -- * Properad Types
    Properad(..)
  , ProperadMorphism(..)
    -- * Corollas
  , Corolla(..)
  , CorollaValue(..)
  , buildCorolla
  , corollaDegIn
  , corollaDegOut
    -- * Properad Composition
  , composeAlong
  , graftCorollas
    -- * Summing Functors
  , ProperadFunctor(..)
  , evaluateOnSubgraph
  , evaluateOnPath
    -- * Power Flow Specific
  , PowerFlowState(..)
  , enforceLocalConservation
  , computeNetInjection
    -- * Topological Processing
  , topologicalOrder
  , processInOrder
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (sortBy)
import Data.Ord (comparing)
import GHC.Generics
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON)

import Chopaan.AC.SummingFunctor (DirectedGraph(..), Node(..), Edge(..), PowerPair(..))

-- | A properad P(m,n) represents processes with m inputs and n outputs.
-- In our case, these are power flow states on half-edges.
data Properad = Properad
  { propDegIn  :: Int                    -- ^ Number of input ports
  , propDegOut :: Int                    -- ^ Number of output ports
  , propState  :: Vector PowerFlowState  -- ^ State on each port (in then out)
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

-- | State of power flow on a single port/half-edge
data PowerFlowState = PowerFlowState
  { pfsP :: Double  -- ^ Real power (kW)
  , pfsQ :: Double  -- ^ Reactive power (kVAR)
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

instance Semigroup PowerFlowState where
  (PowerFlowState p1 q1) <> (PowerFlowState p2 q2) =
    PowerFlowState (p1 + p2) (q1 + q2)

instance Monoid PowerFlowState where
  mempty = PowerFlowState 0 0

-- | Morphism in the properad (composition)
data ProperadMorphism = ProperadMorphism
  { pmSource      :: Properad
  , pmTarget      :: Properad
  , pmSharedPorts :: [(Int, Int)]  -- ^ (output port of source, input port of target)
  } deriving (Show, Eq, Generic, NFData)

-- | Corolla: a vertex with all its incident half-edges
data Corolla = Corolla
  { corVertex   :: Node
  , corInEdges  :: [Edge]   -- ^ Edges with this vertex as target
  , corOutEdges :: [Edge]   -- ^ Edges with this vertex as source
  } deriving (Show, Eq, Generic, NFData)

-- | Value of a summing functor on a corolla
-- This is what the neural network learns for each node
data CorollaValue = CorollaValue
  { cvCorolla    :: Corolla
  , cvInFlows    :: Vector PowerFlowState   -- ^ Flows on incoming edges
  , cvOutFlows   :: Vector PowerFlowState   -- ^ Flows on outgoing edges
  , cvNetInject  :: PowerFlowState          -- ^ Net injection (gen - load)
  } deriving (Show, Eq, Generic, NFData)

-- | Build a corolla from graph structure
buildCorolla :: DirectedGraph -> Node -> Corolla
buildCorolla graph node = Corolla
  { corVertex   = node
  , corInEdges  = filter ((== node) . edgeTarget) (dgEdges graph)
  , corOutEdges = filter ((== node) . edgeSource) (dgEdges graph)
  }

corollaDegIn :: Corolla -> Int
corollaDegIn = length . corInEdges

corollaDegOut :: Corolla -> Int
corollaDegOut = length . corOutEdges

-- | Properad composition: graft two properads along shared ports
--
-- Given P₁ ∈ P(m,k) and P₂ ∈ P(n,r), with ℓ shared ports:
--   P₁ ∘_E P₂ ∈ P(m + n - ℓ, k + r - ℓ)
--
-- The composition identifies outputs of P₁ with inputs of P₂.
composeAlong :: [(Int, Int)]  -- ^ (output of first, input of second)
             -> Properad
             -> Properad
             -> Properad
composeAlong sharedPorts p1 p2 =
  let numShared = length sharedPorts

      -- Identify output flows of p1 with input flows of p2
      -- This is the properad grafting
      sharedFlows = [ (propState p1 V.! (propDegIn p1 + outIdx),
                       propState p2 V.! inIdx)
                    | (outIdx, inIdx) <- sharedPorts ]

      -- Average the shared flows (or could require exact match)
      reconciledFlows = [ PowerFlowState
                            ((pfsP f1 + pfsP f2) / 2)
                            ((pfsQ f1 + pfsQ f2) / 2)
                        | (f1, f2) <- sharedFlows ]

      -- Build new properad with unshared ports + reconciled shared
      -- This is a simplification; full properad would track more structure
      newDegIn = propDegIn p1 + propDegIn p2 - numShared
      newDegOut = propDegOut p1 + propDegOut p2 - numShared

  in Properad
       { propDegIn  = newDegIn
       , propDegOut = newDegOut
       , propState  = V.fromList reconciledFlows  -- Simplified
       }

-- | Graft two corollas along shared edges
graftCorollas :: CorollaValue -> CorollaValue -> Edge -> CorollaValue
graftCorollas upstream downstream sharedEdge =
  -- The outflow from upstream on sharedEdge becomes inflow to downstream
  let upOutIdx = findEdgeIndex sharedEdge (corOutEdges $ cvCorolla upstream)
      downInIdx = findEdgeIndex sharedEdge (corInEdges $ cvCorolla downstream)

      -- Transfer flow
      flowOnEdge = case upOutIdx of
        Just i  -> cvOutFlows upstream V.! i
        Nothing -> mempty

      -- Update downstream's inflows
      newDownInFlows = case downInIdx of
        Just i  -> cvInFlows downstream V.// [(i, flowOnEdge)]
        Nothing -> cvInFlows downstream

  in downstream { cvInFlows = newDownInFlows }
  where
    findEdgeIndex e edges = V.findIndex (== e) (V.fromList edges)

-- | Summing functor with properad structure
-- By Corollary 2.20, determined by values on corollas
data ProperadFunctor = ProperadFunctor
  { pfGraph       :: DirectedGraph
  , pfCorollas    :: Map Node CorollaValue
  , pfSlackNode   :: Node
  } deriving (Show, Eq, Generic, NFData)

-- | Evaluate functor on a subgraph by composing corollas
-- Processes nodes in topological order, grafting along edges
evaluateOnSubgraph :: ProperadFunctor -> [Node] -> Map Edge PowerFlowState
evaluateOnSubgraph pf nodes =
  let topoNodes = topologicalOrder (pfGraph pf) nodes

      -- Process in order, accumulating edge flows
      processNode (edgeFlows, corollaVals) node =
        let corolla = fromMaybe (buildCorolla (pfGraph pf) node)
                               (Map.lookup node (pfCorollas pf) >>= Just . cvCorolla)

            -- Gather incoming flows from already-processed upstream nodes
            inFlows = V.fromList
              [ fromMaybe mempty (Map.lookup e edgeFlows)
              | e <- corInEdges corolla ]

            -- Look up or compute corolla value
            corollaVal = fromMaybe
              (CorollaValue corolla inFlows V.empty mempty)
              (Map.lookup node (pfCorollas pf))

            -- Record outgoing flows
            newEdgeFlows = foldr
              (\(e, i) m -> Map.insert e (cvOutFlows corollaVal V.! i) m)
              edgeFlows
              (zip (corOutEdges corolla) [0..])

        in (newEdgeFlows, Map.insert node corollaVal corollaVals)

  in fst $ foldl processNode (Map.empty, Map.empty) topoNodes

-- | Evaluate along a single path in the graph
evaluateOnPath :: ProperadFunctor -> [Node] -> [PowerFlowState]
evaluateOnPath pf path =
  let edgeFlows = evaluateOnSubgraph pf path
      edges = zipWith (\a b -> findEdge (pfGraph pf) a b) path (tail path)
  in mapMaybe (\me -> me >>= flip Map.lookup edgeFlows) edges
  where
    findEdge g src tgt =
      case filter (\e -> edgeSource e == src && edgeTarget e == tgt) (dgEdges g) of
        (e:_) -> Just e
        []    -> Nothing

-- | Enforce local conservation at a corolla
--
-- sum(outflows) - sum(inflows) = net_injection
--
-- Given n-1 "free" outflows, compute the n-th to satisfy conservation
enforceLocalConservation :: PowerFlowState   -- ^ Net injection (gen - load)
                         -> Vector PowerFlowState  -- ^ Incoming flows
                         -> Vector PowerFlowState  -- ^ Proposed outflows (n-1 of them)
                         -> Vector PowerFlowState  -- ^ Complete outflows (n of them)
enforceLocalConservation netInject inFlows proposedOut =
  let totalIn = V.foldl' (<>) mempty inFlows
      proposedSum = V.foldl' (<>) mempty proposedOut

      -- Required total out = total in + net injection
      requiredTotal = totalIn <> netInject

      -- Last outflow satisfies conservation
      lastFlow = PowerFlowState
        { pfsP = pfsP requiredTotal - pfsP proposedSum
        , pfsQ = pfsQ requiredTotal - pfsQ proposedSum
        }
  in V.snoc proposedOut lastFlow

-- | Compute net injection at a node
computeNetInjection :: Double  -- ^ P generation
                    -> Double  -- ^ P load
                    -> Double  -- ^ Q generation
                    -> Double  -- ^ Q load
                    -> PowerFlowState
computeNetInjection pGen pLoad qGen qLoad = PowerFlowState
  { pfsP = pGen - pLoad
  , pfsQ = qGen - qLoad
  }

-- | Compute topological order of nodes in subgraph
topologicalOrder :: DirectedGraph -> [Node] -> [Node]
topologicalOrder graph nodes =
  let nodeSet = Set.fromList nodes

      -- Compute in-degree within subgraph
      inDegree = Map.fromListWith (+)
        [ (edgeTarget e, 1 :: Int)
        | e <- dgEdges graph
        , Set.member (edgeSource e) nodeSet
        , Set.member (edgeTarget e) nodeSet
        ]

      -- Start with nodes that have no incoming edges from within subgraph
      initialOrder = [ n | n <- nodes, Map.findWithDefault 0 n inDegree == 0 ]

  in sortByDependency graph nodeSet initialOrder []
  where
    sortByDependency _ _ [] acc = reverse acc
    sortByDependency g ns (n:rest) acc =
      let -- Find nodes that depend only on already-processed nodes
          newAcc = n : acc
          processed = Set.fromList newAcc

          -- Successors of n that might now be ready
          successors = [ edgeTarget e
                       | e <- dgEdges g
                       , edgeSource e == n
                       , Set.member (edgeTarget e) ns
                       ]

          -- Check if all predecessors are processed
          isReady node = all (`Set.member` processed)
            [ edgeSource e
            | e <- dgEdges g
            , edgeTarget e == node
            , Set.member (edgeSource e) ns
            ]

          newReady = filter isReady successors

      in sortByDependency g ns (rest ++ newReady) newAcc

-- | Process nodes in topological order, applying a function at each
-- This is the core of properad composition
processInOrder :: DirectedGraph
               -> Node  -- ^ Slack node (skip)
               -> (Node -> Map Edge PowerFlowState -> Map Edge PowerFlowState)
               -> Map Edge PowerFlowState
processInOrder graph slackNode processNode =
  let nodes = dgNodes graph
      topoNodes = topologicalOrder graph nodes
      nonSlack = filter (/= slackNode) topoNodes
  in foldl (\flows n -> processNode n flows) Map.empty nonSlack
