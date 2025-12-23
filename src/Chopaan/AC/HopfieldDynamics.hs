{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE GADTs #-}

-- | Categorical Hopfield Dynamics for Power Flow
--
-- Based on Manin-Marcolli §6: Hopfield dynamics on networks
--
-- Key insight: Power flow can be viewed as a dynamical system where
-- the state evolves toward attractors representing optimal dispatch.
--
-- The categorical Hopfield equation (Eq. 6.2):
--
--   X_e(n+1) = X_e(n) ⊕ (⊕_{e'} T_{ee'}(X_{e'}(n)) ⊕ Θ_e)₊
--
-- Where:
--   X_e     : state on edge e (power flow)
--   T_{ee'} : transition functor (network coupling)
--   Θ_e     : external input (generation/load)
--   (·)₊    : threshold functor (feasibility)
--
-- Fixed points of this dynamics are valid power flow solutions.
-- The neural network learns T to make these fixed points optimal.

module Chopaan.AC.HopfieldDynamics
  ( -- * State Monoid
    FlowState(..)
  , FlowStateMonoid(..)
    -- * Transition Functors
  , TransitionMatrix(..)
  , applyTransition
    -- * Threshold Functor
  , ThresholdConfig(..)
  , categoricalThreshold
  , softThreshold
    -- * External Input
  , ExternalInput(..)
  , inputFromInjections
    -- * Hopfield Dynamics
  , HopfieldConfig(..)
  , HopfieldState(..)
  , hopfieldStep
  , hopfieldEvolve
  , findAttractor
    -- * Energy Function
  , hopfieldEnergy
  , isLocalMinimum
    -- * Power Flow Specific
  , powerFlowHopfield
  , dispatchAsAttractor
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Matrix (Matrix)
import qualified Data.Matrix as M
import GHC.Generics
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON)

import Chopaan.AC.SummingFunctor (DirectedGraph(..), Node(..), Edge(..))
import Chopaan.AC.Properad (PowerFlowState(..))

--------------------------------------------------------------------------------
-- State Monoid
--------------------------------------------------------------------------------

-- | Flow state on an edge - this is the "categorical neuron activation"
data FlowState = FlowState
  { fsP :: Double      -- ^ Real power (kW)
  , fsQ :: Double      -- ^ Reactive power (kVAR)
  , fsV :: Double      -- ^ Voltage magnitude (pu)
  , fsTheta :: Double  -- ^ Voltage angle (rad)
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

-- | Monoid instance for combining flow states
-- This is the ⊕ operation in the Hopfield equation
instance Semigroup FlowState where
  (FlowState p1 q1 v1 t1) <> (FlowState p2 q2 v2 t2) =
    FlowState (p1 + p2) (q1 + q2) (v1 + v2) (t1 + t2)

instance Monoid FlowState where
  mempty = FlowState 0 0 0 0

-- | Wrapper to make the monoid structure explicit
newtype FlowStateMonoid = FlowStateMonoid { getFlowState :: FlowState }
  deriving (Show, Eq, Generic, NFData)

instance Semigroup FlowStateMonoid where
  FlowStateMonoid a <> FlowStateMonoid b = FlowStateMonoid (a <> b)

instance Monoid FlowStateMonoid where
  mempty = FlowStateMonoid mempty

--------------------------------------------------------------------------------
-- Transition Functors T_{ee'}
--------------------------------------------------------------------------------

-- | Transition matrix: T_{ee'} captures how flow on e' influences e
-- In categorical terms, this is a functor C → C for each edge pair
data TransitionMatrix = TransitionMatrix
  { tmMatrix    :: Matrix Double  -- ^ |E| × |E| matrix
  , tmEdgeIndex :: Map Edge Int   -- ^ Edge to index mapping
  , tmDamping   :: Double         -- ^ Damping factor (0 < α < 1)
  } deriving (Show, Eq, Generic, NFData)

-- | Apply transition: ⊕_{e'} T_{ee'}(X_{e'})
applyTransition :: TransitionMatrix
                -> Map Edge FlowState
                -> Map Edge FlowState
applyTransition tm states =
  let n = M.nrows (tmMatrix tm)

      -- Convert states to vectors
      pVec = V.generate n $ \i ->
        case findEdgeByIndex i (tmEdgeIndex tm) of
          Just e  -> fsP (Map.findWithDefault mempty e states)
          Nothing -> 0

      qVec = V.generate n $ \i ->
        case findEdgeByIndex i (tmEdgeIndex tm) of
          Just e  -> fsQ (Map.findWithDefault mempty e states)
          Nothing -> 0

      -- Matrix-vector multiply: T · X
      pNew = matVecMult (tmMatrix tm) pVec
      qNew = matVecMult (tmMatrix tm) qVec

      -- Convert back to map
  in Map.fromList
       [ (e, FlowState (pNew V.! i) (qNew V.! i) 0 0)
       | (e, i) <- Map.toList (tmEdgeIndex tm)
       ]
  where
    findEdgeByIndex :: Int -> Map Edge Int -> Maybe Edge
    findEdgeByIndex idx edgeMap =
      case filter (\(_, i) -> i == idx) (Map.toList edgeMap) of
        ((e, _):_) -> Just e
        []         -> Nothing

    matVecMult :: Matrix Double -> Vector Double -> Vector Double
    matVecMult mat vec = V.generate (M.nrows mat) $ \i ->
      sum [ M.getElem (i+1) (j+1) mat * (vec V.! j)
          | j <- [0 .. M.ncols mat - 1]
          ]

--------------------------------------------------------------------------------
-- Threshold Functor (·)₊
--------------------------------------------------------------------------------

-- | Configuration for the threshold functor
data ThresholdConfig = ThresholdConfig
  { tcPMax     :: Double  -- ^ Max active power per edge (kW)
  , tcQMax     :: Double  -- ^ Max reactive power per edge (kVAR)
  , tcVMin     :: Double  -- ^ Min voltage (pu)
  , tcVMax     :: Double  -- ^ Max voltage (pu)
  , tcSoftness :: Double  -- ^ Softness parameter for differentiable version
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

-- | Categorical threshold: (C)₊ = C if [ρ(C)] ⪰ 0, else 0
-- This is the "firing" condition - only feasible states propagate
categoricalThreshold :: ThresholdConfig -> FlowState -> FlowState
categoricalThreshold cfg state =
  if isFeasible cfg state
    then state
    else mempty
  where
    isFeasible :: ThresholdConfig -> FlowState -> Bool
    isFeasible ThresholdConfig{..} FlowState{..} =
      abs fsP <= tcPMax &&
      abs fsQ <= tcQMax &&
      fsV >= tcVMin &&
      fsV <= tcVMax

-- | Soft threshold for differentiability (used in neural network training)
-- Uses sigmoid to smoothly transition between 0 and 1
softThreshold :: ThresholdConfig -> FlowState -> FlowState
softThreshold cfg state@FlowState{..} =
  let -- Sigmoid activation for each constraint
      beta = tcSoftness cfg

      pFeasible = sigmoid (beta * (tcPMax cfg - abs fsP))
      qFeasible = sigmoid (beta * (tcQMax cfg - abs fsQ))
      vLowFeas  = sigmoid (beta * (fsV - tcVMin cfg))
      vHighFeas = sigmoid (beta * (tcVMax cfg - fsV))

      -- Product of all feasibility factors
      feasibility = pFeasible * qFeasible * vLowFeas * vHighFeas

  in FlowState
       { fsP = fsP * feasibility
       , fsQ = fsQ * feasibility
       , fsV = fsV
       , fsTheta = fsTheta
       }
  where
    sigmoid x = 1 / (1 + exp (-x))

--------------------------------------------------------------------------------
-- External Input Θ
--------------------------------------------------------------------------------

-- | External input at each edge (from generation/load injections)
data ExternalInput = ExternalInput
  { eiEdgeInputs :: Map Edge FlowState  -- ^ Input per edge
  } deriving (Show, Eq, Generic, NFData)

-- | Build external input from node injections
-- Distributes nodal P, Q to incident edges
inputFromInjections :: DirectedGraph
                    -> Map Node (Double, Double)  -- ^ (P, Q) injection per node
                    -> ExternalInput
inputFromInjections graph injections =
  ExternalInput $ Map.fromList
    [ (e, distributeInjection e)
    | e <- dgEdges graph
    ]
  where
    distributeInjection :: Edge -> FlowState
    distributeInjection e =
      let srcInj = Map.findWithDefault (0, 0) (edgeSource e) injections
          tgtInj = Map.findWithDefault (0, 0) (edgeTarget e) injections
          -- Half of each endpoint's injection contributes to this edge
          (pSrc, qSrc) = srcInj
          (pTgt, qTgt) = tgtInj
      in FlowState ((pSrc - pTgt) / 2) ((qSrc - qTgt) / 2) 0 0

--------------------------------------------------------------------------------
-- Hopfield Dynamics
--------------------------------------------------------------------------------

-- | Configuration for Hopfield dynamics
data HopfieldConfig = HopfieldConfig
  { hcTransition :: TransitionMatrix
  , hcThreshold  :: ThresholdConfig
  , hcMaxIter    :: Int
  , hcTolerance  :: Double
  , hcUseSoft    :: Bool  -- ^ Use soft threshold for training
  } deriving (Show, Eq, Generic, NFData)

-- | State of the Hopfield network at time n
data HopfieldState = HopfieldState
  { hsEdgeStates :: Map Edge FlowState
  , hsIteration  :: Int
  , hsConverged  :: Bool
  , hsEnergy     :: Double
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

-- | Single step of Hopfield dynamics
-- X_e(n+1) = X_e(n) ⊕ (⊕_{e'} T_{ee'}(X_{e'}(n)) ⊕ Θ_e)₊
hopfieldStep :: HopfieldConfig
             -> ExternalInput
             -> HopfieldState
             -> HopfieldState
hopfieldStep cfg input state =
  let currentStates = hsEdgeStates state

      -- Apply transition: ⊕_{e'} T_{ee'}(X_{e'}(n))
      transitioned = applyTransition (hcTransition cfg) currentStates

      -- Add external input: ... ⊕ Θ_e
      withInput = Map.unionWith (<>) transitioned (eiEdgeInputs input)

      -- Apply threshold: (...)₊
      thresholdFn = if hcUseSoft cfg
                    then softThreshold (hcThreshold cfg)
                    else categoricalThreshold (hcThreshold cfg)
      thresholded = Map.map thresholdFn withInput

      -- Combine with current state: X_e(n) ⊕ ...
      -- Use damping: X_new = α·X_old + (1-α)·update
      alpha = tmDamping (hcTransition cfg)
      newStates = Map.unionWith
        (\old new -> FlowState
          { fsP = alpha * fsP old + (1 - alpha) * fsP new
          , fsQ = alpha * fsQ old + (1 - alpha) * fsQ new
          , fsV = alpha * fsV old + (1 - alpha) * fsV new
          , fsTheta = alpha * fsTheta old + (1 - alpha) * fsTheta new
          })
        currentStates
        thresholded

      -- Check convergence
      maxDiff = maximum $ Map.elems $ Map.intersectionWith
        (\old new -> abs (fsP old - fsP new) + abs (fsQ old - fsQ new))
        currentStates
        newStates

      converged = maxDiff < hcTolerance cfg

      energy = hopfieldEnergy cfg newStates

  in HopfieldState
       { hsEdgeStates = newStates
       , hsIteration  = hsIteration state + 1
       , hsConverged  = converged
       , hsEnergy     = energy
       }

-- | Evolve Hopfield dynamics until convergence or max iterations
hopfieldEvolve :: HopfieldConfig
               -> ExternalInput
               -> Map Edge FlowState  -- ^ Initial state
               -> HopfieldState
hopfieldEvolve cfg input initial =
  let initialState = HopfieldState initial 0 False (hopfieldEnergy cfg initial)
  in go initialState
  where
    go state
      | hsConverged state = state
      | hsIteration state >= hcMaxIter cfg = state
      | otherwise = go (hopfieldStep cfg input state)

-- | Find attractor starting from initial state
findAttractor :: HopfieldConfig
              -> ExternalInput
              -> Map Edge FlowState
              -> (Map Edge FlowState, Bool)  -- ^ (final state, is stable)
findAttractor cfg input initial =
  let final = hopfieldEvolve cfg input initial
  in (hsEdgeStates final, hsConverged final)

--------------------------------------------------------------------------------
-- Energy Function
--------------------------------------------------------------------------------

-- | Hopfield energy: E = -½ Σ_{ee'} T_{ee'} X_e X_{e'} - Σ_e Θ_e X_e
-- Fixed points are local minima of this energy
hopfieldEnergy :: HopfieldConfig -> Map Edge FlowState -> Double
hopfieldEnergy cfg states =
  let tm = hcTransition cfg
      n = M.nrows (tmMatrix tm)

      -- Extract P values as vector
      pVec = V.generate n $ \i ->
        case findEdgeByIndex i (tmEdgeIndex tm) of
          Just e  -> fsP (Map.findWithDefault mempty e states)
          Nothing -> 0

      -- Quadratic term: -½ xᵀTx
      tTimesP = matVecMult (tmMatrix tm) pVec
      quadratic = -0.5 * V.sum (V.zipWith (*) pVec tTimesP)

  in quadratic
  where
    findEdgeByIndex idx edgeMap =
      case filter (\(_, i) -> i == idx) (Map.toList edgeMap) of
        ((e, _):_) -> Just e
        []         -> Nothing

    matVecMult mat vec = V.generate (M.nrows mat) $ \i ->
      sum [ M.getElem (i+1) (j+1) mat * (vec V.! j)
          | j <- [0 .. M.ncols mat - 1]
          ]

-- | Check if state is a local minimum (attractor)
isLocalMinimum :: HopfieldConfig -> ExternalInput -> Map Edge FlowState -> Bool
isLocalMinimum cfg input states =
  let currentEnergy = hopfieldEnergy cfg states
      nextState = hopfieldStep cfg input
                    (HopfieldState states 0 False currentEnergy)
      nextEnergy = hsEnergy nextState
  in nextEnergy >= currentEnergy - 1e-6

--------------------------------------------------------------------------------
-- Power Flow Specific
--------------------------------------------------------------------------------

-- | Build Hopfield dynamics for a power flow network
powerFlowHopfield :: DirectedGraph
                  -> Map Node (Double, Double)  -- ^ (P_gen - P_load, Q_gen - Q_load)
                  -> ThresholdConfig
                  -> (HopfieldConfig, ExternalInput)
powerFlowHopfield graph injections threshCfg =
  let edges = dgEdges graph
      numEdges = length edges
      edgeIdx = Map.fromList $ zip edges [0..]

      -- Build transition matrix from network topology
      -- T_{ee'} > 0 if edges share a node, weighted by impedance
      transMatrix = M.matrix numEdges numEdges $ \(i, j) ->
        if i == j
          then 0.5  -- Self-connection (inertia)
          else
            let ei = edges !! (i - 1)
                ej = edges !! (j - 1)
            in if sharesNode ei ej
               then 0.1  -- Connected edges influence each other
               else 0

      transition = TransitionMatrix transMatrix edgeIdx 0.3

      config = HopfieldConfig
        { hcTransition = transition
        , hcThreshold  = threshCfg
        , hcMaxIter    = 100
        , hcTolerance  = 1e-4
        , hcUseSoft    = True
        }

      input = inputFromInjections graph injections

  in (config, input)
  where
    sharesNode e1 e2 =
      edgeSource e1 == edgeSource e2 ||
      edgeSource e1 == edgeTarget e2 ||
      edgeTarget e1 == edgeSource e2 ||
      edgeTarget e1 == edgeTarget e2

-- | Find optimal dispatch as the attractor of Hopfield dynamics
-- The neural network learns T such that attractors are optimal
dispatchAsAttractor :: DirectedGraph
                    -> Map Node (Double, Double)
                    -> ThresholdConfig
                    -> (Map Edge FlowState, Bool)
dispatchAsAttractor graph injections threshCfg =
  let (config, input) = powerFlowHopfield graph injections threshCfg

      -- Initialize with zero flows
      initialFlows = Map.fromList
        [ (e, FlowState 0 0 1.0 0)  -- Start at nominal voltage
        | e <- dgEdges graph
        ]

  in findAttractor config input initialFlows
