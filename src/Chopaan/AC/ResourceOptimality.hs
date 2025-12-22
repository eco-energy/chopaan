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
newtype OptimalityFunctor = OptimalityFunctor
  { applyBeta :: PowerResource -> IO PowerFlowConfig
  }

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
dispatchFromResources :: PowerResource -> Map Node Double -> Map Node PowerFlowState
dispatchFromResources resources loads =
  let totalLoad = sum (Map.elems loads)
      totalCapacity = sum (Map.elems $ prGenCapacity resources)

      -- Simple proportional dispatch (real implementation would use OPF)
      dispatchFraction = min 1.0 (totalLoad / totalCapacity)

  in Map.mapWithKey (\node cap ->
       PowerFlowState (cap * dispatchFraction) 0  -- P only, Q=0 for simplicity
     ) (prGenCapacity resources)

-- | Extract resource usage from a dispatch
resourcesFromDispatch :: Map Node PowerFlowState -> PowerResource
resourcesFromDispatch dispatch = PowerResource
  { prGenCapacity = Map.map pfsP dispatch
  , prLineRating = Map.empty  -- Would need edge info
  , prVoltageRange = (0.95, 1.05)  -- Default
  , prCostCoeffs = Map.empty
  , prReserveMargin = 0
  }
