{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- | Resource Optimization via Adjoint Functors
--
-- Based on Manin-Marcolli §3.3: Adjunction and optimality of resources
--
-- The key insight: optimization problems ARE adjoint functors.
--
-- Given:
--   ρ : C → R   (assigns resources to configurations)
--   β : R → C   (finds optimal config given resources) -- LEFT ADJOINT
--
-- The adjunction MorC(β(A), C) ≃ MorR(A, ρ(C)) means:
--   Any way to convert resources A to what config C needs
--   factors uniquely through the OPTIMAL config β(A).
--
-- For power flow:
--   C = PowerFlowConfig (edge flows, voltages, setpoints)
--   R = Resources (generation capacity, line ratings, costs)
--   ρ(config) = resources consumed by this config
--   β(resources) = optimal dispatch given available resources
--
-- The neural network learns to approximate β.

module Chopaan.AC.ResourceOptimality
  ( -- * Resource Categories
    Resource(..)
  , ResourceMorphism(..)
  , PowerFlowConfig(..)
  , ConfigMorphism(..)
    -- * The Adjunction
  , ResourceFunctor(..)      -- ρ : C → R
  , OptimalityFunctor(..)    -- β : R → C (left adjoint)
  , defaultOptimalityFunctor
  , AdjunctionWitness(..)
    -- * Optimization as Universal Property
  , isOptimal
  , factorsThroughOptimal
    -- * Meshed Grid Support
  , MeshedGraph(..)
  , CycleDecomposition(..)
  , decomposeCycles
  , iterateToConvergence
    -- * Concrete Implementation
  , PowerResource(..)
  , dispatchFromResources
  , resourcesFromDispatch
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List (sortBy)
import Data.Ord (comparing)
import GHC.Generics
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON)

import Chopaan.AC.SummingFunctor (DirectedGraph(..), Node(..), Edge(..), PowerPair(..))
import Chopaan.AC.Properad (PowerFlowState(..), Corolla(..))

--------------------------------------------------------------------------------
-- Resource Category R
--------------------------------------------------------------------------------

-- | Resources available to the power system
data PowerResource = PowerResource
  { prGenCapacity   :: Map Node Double      -- ^ Max generation at each node (kW)
  , prLineRating    :: Map Edge Double      -- ^ Thermal limit per line (kVA)
  , prVoltageRange  :: (Double, Double)     -- ^ (V_min, V_max) in pu
  , prCostCoeffs    :: Map Node (Double, Double, Double)  -- ^ (a, b, c) for aP² + bP + c
  , prReserveMargin :: Double               -- ^ Required reserve (fraction)
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

-- | Morphism in resource category: resource conversion/allocation
data ResourceMorphism = ResourceMorphism
  { rmSource :: PowerResource
  , rmTarget :: PowerResource
  , rmConversion :: Map Node Double  -- ^ How much of each resource is converted
  } deriving (Show, Eq, Generic, NFData)

-- | Check if resources A can be converted to resources B
-- (A morphism exists iff A has "enough" of everything B needs)
canConvert :: PowerResource -> PowerResource -> Bool
canConvert a b = and
  [ Map.findWithDefault 0 n (prGenCapacity a) >= Map.findWithDefault 0 n (prGenCapacity b)
  | n <- Map.keys (prGenCapacity b)
  ]

--------------------------------------------------------------------------------
-- Configuration Category C
--------------------------------------------------------------------------------

-- | Power flow configuration (what the system is actually doing)
data PowerFlowConfig = PowerFlowConfig
  { pfcEdgeFlows   :: Map Edge PowerFlowState   -- ^ (P, Q) on each edge
  , pfcNodeVoltage :: Map Node (Double, Double) -- ^ (|V|, θ) at each node
  , pfcGeneration  :: Map Node PowerFlowState   -- ^ (P, Q) generated at each node
  , pfcCurtailment :: Map Node Double           -- ^ Power curtailed at each node
  , pfcLosses      :: Double                    -- ^ Total system losses
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

-- | Morphism in config category: system state transition
data ConfigMorphism = ConfigMorphism
  { cmSource :: PowerFlowConfig
  , cmTarget :: PowerFlowConfig
  , cmCost   :: Double  -- ^ Cost of this transition (switching, ramping, etc.)
  } deriving (Show, Eq, Generic, NFData)

--------------------------------------------------------------------------------
-- The Functors
--------------------------------------------------------------------------------

-- | ρ : C → R  (assigns resources to configurations)
-- Given a power flow config, what resources does it use?
newtype ResourceFunctor = ResourceFunctor
  { applyRho :: PowerFlowConfig -> PowerResource
  }

-- | Default resource functor: extract resource usage from config
defaultResourceFunctor :: ResourceFunctor
defaultResourceFunctor = ResourceFunctor $ \config ->
  PowerResource
    { prGenCapacity = Map.map pfsP (pfcGeneration config)
    , prLineRating = Map.map (\pf -> sqrt (pfsP pf ^ 2 + pfsQ pf ^ 2)) (pfcEdgeFlows config)
    , prVoltageRange = let vs = map fst (Map.elems $ pfcNodeVoltage config)
                       in (minimum vs, maximum vs)
    , prCostCoeffs = Map.empty  -- Not used in reverse direction
    , prReserveMargin = 0
    }

-- | β : R → C  (optimal config given resources) -- LEFT ADJOINT
-- This is what the neural network learns!
--
-- In practice, applyBeta should call a trained neural network that has learned
-- the optimal dispatch policy including transmission constraints, voltage limits,
-- and N-1 security criteria. The network approximates the solution to the
-- full AC optimal power flow (OPF) problem.
--
-- For a fallback implementation without a trained network, use defaultOptimalityFunctor
-- which performs economic dispatch based on generator cost coefficients.
newtype OptimalityFunctor = OptimalityFunctor
  { applyBeta :: PowerResource -> IO PowerFlowConfig
  }

-- | Default optimality functor: economic dispatch without neural network
--
-- This is a fallback implementation that performs merit-order economic dispatch
-- based on generator cost coefficients. It does NOT account for:
--   - Transmission constraints (line flow limits)
--   - Voltage constraints
--   - AC power flow equations
--   - N-1 security criteria
--
-- In practice, you should use a trained neural network via applyBeta to get
-- a dispatch that respects all physical and operational constraints.
defaultOptimalityFunctor :: DirectedGraph -> Map Node Double -> OptimalityFunctor
defaultOptimalityFunctor graph loads = OptimalityFunctor $ \resources -> do
  -- Perform economic dispatch using merit order
  let dispatch = dispatchFromResources resources loads

      -- Compute total generation and losses (simplified)
      totalGen = sum [pfsP pf | pf <- Map.elems dispatch]
      totalLoad = sum (Map.elems loads)
      losses = max 0 (totalGen - totalLoad)

      -- Create voltage profile (flat start at 1.0 pu)
      nodeVoltages = Map.fromList
        [(node, (1.0, 0.0)) | node <- dgNodes graph]

      -- Compute edge flows (simplified DC approximation)
      edgeFlows = Map.fromList
        [(edge, estimateFlow edge) | edge <- dgEdges graph]

      estimateFlow edge =
        let src = edgeSource edge
            tgt = edgeTarget edge
            srcP = maybe 0 pfsP (Map.lookup src dispatch)
            tgtP = maybe 0 pfsP (Map.lookup tgt dispatch)
            flowP = (srcP - tgtP) / 2
            flowQ = 0  -- Simplified: ignore reactive power
        in PowerFlowState flowP flowQ

      -- No curtailment in this simple model
      curtailment = Map.empty

      config = PowerFlowConfig
        { pfcEdgeFlows = edgeFlows
        , pfcNodeVoltage = nodeVoltages
        , pfcGeneration = dispatch
        , pfcCurtailment = curtailment
        , pfcLosses = losses
        }

  return config

--------------------------------------------------------------------------------
-- Adjunction Witness
--------------------------------------------------------------------------------

-- | Witness that β ⊣ ρ (β is left adjoint to ρ)
--
-- The adjunction says: MorC(β(A), C) ≃ MorR(A, ρ(C))
--
-- In words: ways to modify optimal-config-for-A into config-C
--           correspond to ways to convert resources-A into resources-for-C
data AdjunctionWitness = AdjunctionWitness
  { -- | Given f : A → ρ(C), produce g : β(A) → C
    awTranspose :: ResourceMorphism -> PowerFlowConfig -> ConfigMorphism
    -- | Given g : β(A) → C, produce f : A → ρ(C)
  , awUntranspose :: ConfigMorphism -> PowerResource -> ResourceMorphism
  }

-- | Check if a config is optimal for given resources
-- Optimal means: it's in the image of β, and any other config
-- with the same resources has higher cost
isOptimal :: OptimalityFunctor -> ResourceFunctor -> PowerResource -> PowerFlowConfig -> IO Bool
isOptimal beta rho resources config = do
  optimalConfig <- applyBeta beta resources
  let optCost = pfcLosses optimalConfig + sum (Map.elems $ pfcCurtailment optimalConfig)
      thisCost = pfcLosses config + sum (Map.elems $ pfcCurtailment config)
  return $ thisCost <= optCost * 1.001  -- Within 0.1% of optimal

-- | The universal property: any morphism A → ρ(C) factors through β(A)
-- This is what makes β the LEFT adjoint
factorsThroughOptimal :: OptimalityFunctor
                      -> ResourceFunctor
                      -> PowerResource
                      -> PowerFlowConfig
                      -> IO (Maybe (ConfigMorphism, ResourceMorphism))
factorsThroughOptimal beta rho resources targetConfig = do
  optimalConfig <- applyBeta beta resources
  let targetResources = applyRho rho targetConfig

  if canConvert resources targetResources
    then do
      -- The factorization exists:
      -- resources → ρ(targetConfig) factors as:
      -- resources → ρ(β(resources)) → ρ(targetConfig)
      let configMorphism = ConfigMorphism optimalConfig targetConfig 0
          resourceMorphism = ResourceMorphism resources targetResources Map.empty
      return $ Just (configMorphism, resourceMorphism)
    else
      return Nothing

--------------------------------------------------------------------------------
-- Meshed Grid Support
--------------------------------------------------------------------------------

-- | Graph that may have cycles
data MeshedGraph = MeshedGraph
  { mgNodes :: [Node]
  , mgEdges :: [Edge]
  , mgCycles :: [[Edge]]  -- ^ Fundamental cycles
  } deriving (Show, Eq, Generic, NFData)

-- | Decomposition into tree + cycle edges
data CycleDecomposition = CycleDecomposition
  { cdSpanningTree :: [Edge]      -- ^ Tree edges (processed by properad)
  , cdCycleEdges   :: [Edge]      -- ^ Non-tree edges (need iteration)
  , cdFundamentalCycles :: [[Edge]]  -- ^ Cycles formed by each non-tree edge
  } deriving (Show, Eq, Generic, NFData)

-- | Find spanning tree and fundamental cycles
decomposeCycles :: DirectedGraph -> CycleDecomposition
decomposeCycles graph =
  let nodes = dgNodes graph
      edges = dgEdges graph

      -- Build spanning tree via DFS
      (treeEdges, nonTreeEdges) = spanningTreeDFS nodes edges

      -- Each non-tree edge creates a fundamental cycle
      cycles = [ findCycle treeEdges e | e <- nonTreeEdges ]

  in CycleDecomposition
       { cdSpanningTree = treeEdges
       , cdCycleEdges = nonTreeEdges
       , cdFundamentalCycles = cycles
       }
  where
    spanningTreeDFS :: [Node] -> [Edge] -> ([Edge], [Edge])
    spanningTreeDFS nodes edges =
      let visited = Set.empty
          (tree, _, nonTree) = dfs (head nodes) visited [] edges
      in (tree, nonTree)

    dfs :: Node -> Set Node -> [Edge] -> [Edge] -> ([Edge], Set Node, [Edge])
    dfs node visited treeAcc remaining
      | Set.member node visited = (treeAcc, visited, remaining)
      | otherwise =
          let visited' = Set.insert node visited
              (outEdges, rest) = partition (\e -> edgeSource e == node) remaining
              (treeEdges, nonTree) = partition (\e -> not $ Set.member (edgeTarget e) visited') outEdges
          in foldr (\e (t, v, r) -> dfs (edgeTarget e) v (e:t) r)
                   (treeAcc, visited', rest ++ nonTree)
                   treeEdges

    partition p xs = (filter p xs, filter (not . p) xs)

    findCycle :: [Edge] -> Edge -> [Edge]
    findCycle treeEdges cycleEdge =
      -- Find path in tree from target to source of cycleEdge
      cycleEdge : pathInTree treeEdges (edgeTarget cycleEdge) (edgeSource cycleEdge)

    pathInTree :: [Edge] -> Node -> Node -> [Edge]
    pathInTree _ from to | from == to = []
    pathInTree edges from to =
      case filter (\e -> edgeSource e == from) edges of
        [] -> []  -- No path (shouldn't happen in tree)
        (e:_) -> e : pathInTree edges (edgeTarget e) to

-- | Iterate corolla updates until convergence (for meshed grids)
iterateToConvergence
  :: (Node -> Map Edge PowerFlowState -> Map Edge PowerFlowState)  -- ^ Corolla update
  -> DirectedGraph
  -> Int            -- ^ Max iterations
  -> Double         -- ^ Tolerance
  -> Map Edge PowerFlowState  -- ^ Initial flows
  -> (Map Edge PowerFlowState, Int, Bool)  -- ^ (final flows, iterations, converged)
iterateToConvergence updateNode graph maxIter tol initialFlows =
  go 0 initialFlows
  where
    go iter flows
      | iter >= maxIter = (flows, iter, False)
      | otherwise =
          let newFlows = foldr updateNode flows (dgNodes graph)
              maxDiff = maximum [ abs (pfsP (newFlows Map.! e) - pfsP (flows Map.! e))
                                | e <- dgEdges graph, Map.member e flows, Map.member e newFlows ]
          in if maxDiff < tol
             then (newFlows, iter + 1, True)
             else go (iter + 1) newFlows

--------------------------------------------------------------------------------
-- Concrete Implementation for Power Systems
--------------------------------------------------------------------------------

-- | Dispatch P, Q at each generator given available resources
--
-- Implements economic dispatch using merit order based on generator cost coefficients.
-- Cost function: C(P) = a*P² + b*P + c
-- Marginal cost: dC/dP = 2*a*P + b
--
-- Algorithm: Sort generators by marginal cost at zero output (coefficient 'b'),
-- then dispatch cheapest generators first up to their capacity (merit order).
dispatchFromResources :: PowerResource -> Map Node Double -> Map Node PowerFlowState
dispatchFromResources resources loads =
  let totalLoad = sum (Map.elems loads)
      totalCapacity = sum (Map.elems $ prGenCapacity resources)

      -- If insufficient capacity, scale down load
      actualLoad = min totalLoad totalCapacity

      -- Merit order dispatch: sort generators by marginal cost at zero output
      genList = Map.toList (prGenCapacity resources)

      -- Sort by 'b' coefficient (marginal cost at P=0) from cost function a*P² + b*P + c
      -- Generators without cost coefficients are assigned high cost (dispatched last)
      sortedGens = sortBy (\(n1, _) (n2, _) ->
        let (_, b1, _) = Map.findWithDefault (0, 1000, 0) n1 (prCostCoeffs resources)
            (_, b2, _) = Map.findWithDefault (0, 1000, 0) n2 (prCostCoeffs resources)
        in compare b1 b2
        ) genList

      -- Dispatch generators in merit order: fill cheapest first
      (dispatchMap, _remaining) = foldl
        (\(accMap, remainingLoad) (node, capacity) ->
          let dispatchHere = min capacity remainingLoad
              newRemaining = remainingLoad - dispatchHere
          in (Map.insert node (PowerFlowState dispatchHere 0) accMap, newRemaining)
        )
        (Map.empty, actualLoad)
        sortedGens

      -- Add zero dispatch for any generators not needed
      fullDispatch = foldr
        (\(node, _) accMap ->
          if Map.member node accMap
            then accMap
            else Map.insert node (PowerFlowState 0 0) accMap
        )
        dispatchMap
        genList

  in fullDispatch

-- | Extract resource usage from a dispatch
--
-- Takes the network graph to compute approximate line flows from nodal injections.
-- Line flow approximation: for each edge (i→j), estimate flow based on the
-- difference in injections at nodes i and j.
resourcesFromDispatch :: DirectedGraph -> Map Node PowerFlowState -> PowerResource
resourcesFromDispatch graph dispatch = PowerResource
  { prGenCapacity = Map.map pfsP dispatch
  , prLineRating = computeLineRatings graph dispatch
  , prVoltageRange = (0.95, 1.05)  -- Default range
  , prCostCoeffs = Map.empty
  , prReserveMargin = 0
  }
  where
    -- Compute approximate line ratings from nodal dispatch
    computeLineRatings :: DirectedGraph -> Map Node PowerFlowState -> Map Edge Double
    computeLineRatings graph dispatch =
      Map.fromList
        [ (edge, estimateEdgeFlow edge)
        | edge <- dgEdges graph
        ]

    -- Estimate flow on edge based on source and target node injections
    estimateEdgeFlow :: Edge -> Double
    estimateEdgeFlow edge =
      let src = edgeSource edge
          tgt = edgeTarget edge
          srcP = maybe 0 pfsP (Map.lookup src dispatch)
          tgtP = maybe 0 pfsP (Map.lookup tgt dispatch)
          -- Approximate flow as proportional to injection difference
          -- In a real network, would solve power flow equations
          flowP = abs (srcP - tgtP) / 2
      in flowP
