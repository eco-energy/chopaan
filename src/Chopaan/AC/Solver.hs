{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

-- | AC Optimal Power Flow Solver using IPOPT
--
-- Full AC-OPF for grid-tied solar at 220V nominal (160-270V Pakistan range),
-- WAPDA sync, phase coupling. Uses IPOPT for nonlinear optimization.
--
-- Objective: Minimize losses + curtailment penalty
--
-- @
-- min Σ (I²R losses) + λ × Σ (P_curtailed)
-- @
--
-- Subject to AC power flow equations and operational constraints.

module Chopaan.AC.Solver
  ( -- * Types
    NodeId(..)
  , NodeType(..)
  , NodeState(..)
  , EdgeParams(..)
  , ACProblem(..)
  , ACSolution(..)
  , OptimalDispatch(..)
  , SolarProfile(..)
  , LoadProfile(..)
    -- * Solver
  , solveACOPF
    -- * Dataset Generation
  , sampleFromProfiles
  , generateRealisticDataset
    -- * Profiles
  , defaultSolarProfile
  , defaultLoadProfile
  , sampleTimeOfDay
  ) where

#ifndef ghcjs_HOST_OS

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad (forM, replicateM)
import System.Random (randomRIO, randomIO)
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Process (readProcess)
import System.IO.Temp (withSystemTempFile)
import System.IO (hClose, hPutStr)
import Text.Printf (printf)

-- | Node identifier in the network
newtype NodeId = NodeId Int
  deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON, Num)

-- | Node type for dispatch
data NodeType
  = SlackBus        -- ^ WAPDA connection (fixed V, absorbs P/Q imbalance)
  | PVBus           -- ^ Solar inverter (controls P, Q within limits)
  | PQBus           -- ^ Load bus (fixed P, Q demand)
  deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

-- | Node electrical state and limits
data NodeState = NodeState
  { nodeType      :: NodeType
  , nodeV         :: Double    -- ^ Voltage magnitude (V), nominal 220
  , nodeDelta     :: Double    -- ^ Voltage angle (radians)
  , nodePGen      :: Double    -- ^ Real power generation (kW)
  , nodeQGen      :: Double    -- ^ Reactive power generation (kVAR)
  , nodePLoad     :: Double    -- ^ Real power load (kW)
  , nodeQLoad     :: Double    -- ^ Reactive power load (kVAR)
  , nodePMax      :: Double    -- ^ Max P generation (solar capacity)
  , nodeQMax      :: Double    -- ^ Max Q generation (inverter limit, ~0.3 × S_rated)
  , nodeSRated    :: Double    -- ^ Inverter apparent power rating (kVA)
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

-- | Edge parameters (transmission line characteristics)
data EdgeParams = EdgeParams
  { edgeR       :: Double      -- ^ Resistance (Ω)
  , edgeX       :: Double      -- ^ Reactance (Ω)
  , edgeCap     :: Double      -- ^ Thermal capacity (kVA)
  , edgeLength  :: Double      -- ^ Length in meters
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

-- | Full AC-OPF problem specification
data ACProblem = ACProblem
  { acNodes       :: [NodeId]
  , acSlackNode   :: NodeId
  , acNodeState   :: Map NodeId NodeState
  , acEdges       :: [(NodeId, NodeId)]
  , acEdgeParams  :: Map (NodeId, NodeId) EdgeParams
  , acVMin        :: Double                         -- ^ 160V
  , acVMax        :: Double                         -- ^ 270V
  , acFreqNominal :: Double                         -- ^ 50 Hz
  , acFreqMin     :: Double                         -- ^ 49.5 Hz
  , acFreqMax     :: Double                         -- ^ 50.5 Hz
  , acCurtailPenalty :: Double                      -- ^ λ for curtailment cost
  } deriving (Show, Generic, NFData, ToJSON, FromJSON)

-- | Optimal dispatch solution
data OptimalDispatch = OptimalDispatch
  { odPSetpoints  :: Map NodeId Double    -- ^ P setpoint per PV bus (kW)
  , odQSetpoints  :: Map NodeId Double    -- ^ Q setpoint per PV bus (kVAR)
  , odCurtailed   :: Map NodeId Double    -- ^ Curtailed power per node (kW)
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

-- | Solution to AC-OPF problem
data ACSolution = ACSolution
  { solVoltages   :: Map NodeId (Double, Double)     -- ^ (V, δ) for each node
  , solPFlows     :: Map (NodeId, NodeId) Double     -- ^ Real power flow per edge
  , solQFlows     :: Map (NodeId, NodeId) Double     -- ^ Reactive power flow per edge
  , solLosses     :: Double                          -- ^ Total I²R losses (kW)
  , solDispatch   :: OptimalDispatch                 -- ^ Optimal P,Q setpoints
  , solObjective  :: Double                          -- ^ Objective value
  , solFeasible   :: Bool
  , solIterations :: Int                             -- ^ IPOPT iterations
  } deriving (Show, Generic, NFData, ToJSON, FromJSON)

-- | Solar generation profile (normalized 0-1 by hour)
newtype SolarProfile = SolarProfile { unSolarProfile :: [Double] }
  deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

-- | Load profile (normalized to average by hour)
newtype LoadProfile = LoadProfile { unLoadProfile :: [Double] }
  deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

-- | Default Pakistan solar profile (June, clear sky, Lahore ~31°N)
-- Hours 0-23, normalized to peak=1.0
defaultSolarProfile :: SolarProfile
defaultSolarProfile = SolarProfile
  [ 0.00, 0.00, 0.00, 0.00, 0.00, 0.05  -- 0-5
  , 0.20, 0.45, 0.70, 0.88, 0.96, 1.00  -- 6-11
  , 0.98, 0.92, 0.82, 0.68, 0.50, 0.28  -- 12-17
  , 0.08, 0.00, 0.00, 0.00, 0.00, 0.00  -- 18-23
  ]

-- | Default Pakistan residential load profile
-- Peak at 7-9 PM (AC, cooking, TV), secondary peak at noon
defaultLoadProfile :: LoadProfile
defaultLoadProfile = LoadProfile
  [ 0.35, 0.30, 0.28, 0.28, 0.30, 0.40  -- 0-5 (night, early morning)
  , 0.55, 0.70, 0.65, 0.60, 0.65, 0.75  -- 6-11 (morning activity)
  , 0.85, 0.80, 0.75, 0.70, 0.75, 0.90  -- 12-17 (afternoon, AC load)
  , 1.00, 1.10, 1.05, 0.85, 0.60, 0.45  -- 18-23 (evening peak)
  ]

-- | Sample time of day with realistic distribution (more samples during solar hours)
sampleTimeOfDay :: IO Double
sampleTimeOfDay = do
  -- Bias towards solar hours (6 AM - 6 PM) for training
  biased <- randomIO :: IO Bool
  if biased
    then randomRIO (6.0, 18.0)   -- 60% solar hours
    else randomRIO (0.0, 24.0)   -- 40% full day

-- | Solve AC-OPF using IPOPT (via Python/CasADi interface)
--
-- Objective: min Σ losses + λ × Σ curtailment
--
-- Expected runtime: ~0.5-2s per solve
solveACOPF :: ACProblem -> IO (Maybe ACSolution)
solveACOPF prob = withSystemTempFile "acopf_problem.json" $ \probPath probH -> do
  hPutStr probH (BL.unpack $ encode prob)
  hClose probH

  -- Call IPOPT via Python/CasADi
  result <- readProcess "python3" ["-c", ipoptSolverScript, probPath] ""

  case decode (BL.pack result) of
    Just sol -> return $ Just sol
    Nothing -> do
      putStrLn $ "IPOPT solve failed: " ++ take 200 result
      return Nothing

-- | Python script using CasADi + IPOPT for AC-OPF
ipoptSolverScript :: String
ipoptSolverScript = unlines
  [ "import sys, json"
  , "import numpy as np"
  , "try:"
  , "    import casadi as ca"
  , "except ImportError:"
  , "    print(json.dumps(None))"
  , "    sys.exit(0)"
  , ""
  , "with open(sys.argv[1]) as f:"
  , "    prob = json.load(f)"
  , ""
  , "nodes = prob['acNodes']"
  , "n = len(nodes)"
  , "slack_idx = prob['acSlackNode']"
  , "v_min, v_max = prob['acVMin'], prob['acVMax']"
  , "curtail_penalty = prob['acCurtailPenalty']"
  , ""
  , "# Build node data"
  , "node_state = prob['acNodeState']"
  , "p_load = np.array([node_state[str(i)]['nodePLoad'] for i in range(n)])"
  , "q_load = np.array([node_state[str(i)]['nodeQLoad'] for i in range(n)])"
  , "p_max = np.array([node_state[str(i)]['nodePMax'] for i in range(n)])"
  , "q_max = np.array([node_state[str(i)]['nodeQMax'] for i in range(n)])"
  , "s_rated = np.array([node_state[str(i)]['nodeSRated'] for i in range(n)])"
  , "node_types = [node_state[str(i)]['nodeType'] for i in range(n)]"
  , ""
  , "# Build edge data"
  , "edges = [(e[0], e[1]) for e in prob['acEdges']]"
  , "edge_params = prob['acEdgeParams']"
  , ""
  , "# Decision variables"
  , "opti = ca.Opti()"
  , "V = opti.variable(n)      # Voltage magnitudes"
  , "delta = opti.variable(n)  # Voltage angles"
  , "P_gen = opti.variable(n)  # Real power generation"
  , "Q_gen = opti.variable(n)  # Reactive power generation"
  , "P_curt = opti.variable(n) # Curtailed power"
  , ""
  , "# Bounds"
  , "for i in range(n):"
  , "    if node_types[i] == 'SlackBus':"
  , "        opti.subject_to(V[i] == 220)  # Fixed voltage"
  , "        opti.subject_to(delta[i] == 0)  # Reference angle"
  , "    else:"
  , "        opti.subject_to(opti.bounded(v_min, V[i], v_max))"
  , "        opti.subject_to(opti.bounded(-0.52, delta[i], 0.52))  # ±30°"
  , ""
  , "    if node_types[i] == 'PVBus':"
  , "        opti.subject_to(opti.bounded(0, P_gen[i], p_max[i]))"
  , "        opti.subject_to(opti.bounded(-q_max[i], Q_gen[i], q_max[i]))"
  , "        opti.subject_to(P_gen[i]**2 + Q_gen[i]**2 <= s_rated[i]**2)  # Inverter capacity"
  , "        opti.subject_to(opti.bounded(0, P_curt[i], p_max[i]))"
  , "        opti.subject_to(P_gen[i] + P_curt[i] == p_max[i])  # Available - curtailed = dispatched"
  , "    else:"
  , "        opti.subject_to(P_gen[i] == 0)"
  , "        opti.subject_to(Q_gen[i] == 0)"
  , "        opti.subject_to(P_curt[i] == 0)"
  , ""
  , "# Build Y-bus (admittance matrix)"
  , "Y = np.zeros((n, n), dtype=complex)"
  , "for (i, j) in edges:"
  , "    key = f'({i}, {j})'"
  , "    if key not in edge_params:"
  , "        key = f'({j}, {i})'"
  , "    ep = edge_params[key]"
  , "    r, x = ep['edgeR'], ep['edgeX']"
  , "    y = 1 / complex(r, x)"
  , "    Y[i, i] += y"
  , "    Y[j, j] += y"
  , "    Y[i, j] -= y"
  , "    Y[j, i] -= y"
  , ""
  , "G = np.real(Y)"
  , "B = np.imag(Y)"
  , ""
  , "# Power flow equations"
  , "for i in range(n):"
  , "    P_inj = P_gen[i] - p_load[i]"
  , "    Q_inj = Q_gen[i] - q_load[i]"
  , ""
  , "    P_calc = 0"
  , "    Q_calc = 0"
  , "    for j in range(n):"
  , "        angle_diff = delta[i] - delta[j]"
  , "        P_calc += V[i] * V[j] * (G[i,j] * ca.cos(angle_diff) + B[i,j] * ca.sin(angle_diff))"
  , "        Q_calc += V[i] * V[j] * (G[i,j] * ca.sin(angle_diff) - B[i,j] * ca.cos(angle_diff))"
  , ""
  , "    if node_types[i] != 'SlackBus':"
  , "        opti.subject_to(P_inj == P_calc)"
  , "        opti.subject_to(Q_inj == Q_calc)"
  , ""
  , "# Objective: minimize losses + curtailment penalty"
  , "losses = 0"
  , "for (i, j) in edges:"
  , "    key = f'({i}, {j})'"
  , "    if key not in edge_params:"
  , "        key = f'({j}, {i})'"
  , "    r = edge_params[key]['edgeR']"
  , "    # I² = (P² + Q²) / V² approximately"
  , "    # Loss ≈ r × (P_flow² + Q_flow²) / V²"
  , "    # Simplified: use voltage difference squared as proxy"
  , "    losses += r * ((V[i] - V[j])**2 + (delta[i] - delta[j])**2 * 220**2) / 220**2"
  , ""
  , "curtailment = ca.sum1(P_curt)"
  , "objective = losses + curtail_penalty * curtailment"
  , "opti.minimize(objective)"
  , ""
  , "# Initial guess"
  , "opti.set_initial(V, 220)"
  , "opti.set_initial(delta, 0)"
  , "opti.set_initial(P_gen, p_max / 2)"
  , "opti.set_initial(Q_gen, 0)"
  , "opti.set_initial(P_curt, 0)"
  , ""
  , "# Solve with IPOPT"
  , "opts = {'ipopt.print_level': 0, 'print_time': 0, 'ipopt.max_iter': 200}"
  , "opti.solver('ipopt', opts)"
  , ""
  , "try:"
  , "    sol = opti.solve()"
  , "    V_sol = sol.value(V)"
  , "    delta_sol = sol.value(delta)"
  , "    P_gen_sol = sol.value(P_gen)"
  , "    Q_gen_sol = sol.value(Q_gen)"
  , "    P_curt_sol = sol.value(P_curt)"
  , "    obj_val = sol.value(objective)"
  , "    iterations = sol.stats()['iter_count']"
  , ""
  , "    # Build result"
  , "    result = {"
  , "        'solVoltages': {str(i): [float(V_sol[i]), float(delta_sol[i])] for i in range(n)},"
  , "        'solPFlows': {},"
  , "        'solQFlows': {},"
  , "        'solLosses': float(sol.value(losses)),"
  , "        'solDispatch': {"
  , "            'odPSetpoints': {str(i): float(P_gen_sol[i]) for i in range(n) if node_types[i] == 'PVBus'},"
  , "            'odQSetpoints': {str(i): float(Q_gen_sol[i]) for i in range(n) if node_types[i] == 'PVBus'},"
  , "            'odCurtailed': {str(i): float(P_curt_sol[i]) for i in range(n) if node_types[i] == 'PVBus'}"
  , "        },"
  , "        'solObjective': float(obj_val),"
  , "        'solFeasible': True,"
  , "        'solIterations': int(iterations)"
  , "    }"
  , "    print(json.dumps(result))"
  , "except Exception as e:"
  , "    print(json.dumps({'solFeasible': False, 'error': str(e)}))"
  ]

-- | Sample a problem instance using realistic solar/load profiles
sampleFromProfiles :: Int           -- ^ Number of nodes
                   -> Int           -- ^ Connectivity factor
                   -> SolarProfile
                   -> LoadProfile
                   -> IO (ACProblem, Double)  -- ^ (Problem, hour of day)
sampleFromProfiles nNodes k solar load = do
  -- Sample time of day
  hour <- sampleTimeOfDay
  let hourIdx = floor hour `mod` 24
      solarFrac = unSolarProfile solar !! hourIdx
      loadMult = unLoadProfile load !! hourIdx

  -- Add weather noise (clouds)
  cloudFactor <- randomRIO (0.7, 1.0)
  let effectiveSolar = solarFrac * cloudFactor

  let nodes = [NodeId i | i <- [0..nNodes-1]]
      edges = [(NodeId i, NodeId j) | i <- [0..nNodes-1], j <- [0..nNodes-1],
               i /= j, abs(i-j) <= k]

  -- Generate node states with profiles
  nodeStates <- forM (zip [0..] nodes) $ \(idx, n) -> do
    -- First node is slack (WAPDA), others are PV or PQ
    let nodeT = if idx == 0 then SlackBus
                else if idx `mod` 3 /= 2 then PVBus  -- 2/3 have solar
                else PQBus

    -- Solar capacity (kW peak) - varies by household
    solarCap <- if nodeT == PVBus
                then randomRIO (3.0, 10.0)  -- 3-10 kW systems
                else return 0

    -- Current solar generation based on time and clouds
    let pGen = solarCap * effectiveSolar

    -- Base load (kW) scaled by time of day
    baseLoad <- randomRIO (1.0, 4.0)
    let pLoad = baseLoad * loadMult

    -- Reactive load (inductive - motors, AC compressors)
    let qLoad = pLoad * 0.2  -- ~0.98 power factor

    -- Inverter rating (slightly larger than solar capacity)
    let sRated = if nodeT == PVBus then solarCap * 1.1 else 0
        qMax = if nodeT == PVBus then sRated * 0.3 else 0  -- 30% Q capability

    return (n, NodeState
      { nodeType = nodeT
      , nodeV = 220
      , nodeDelta = 0
      , nodePGen = pGen
      , nodeQGen = 0
      , nodePLoad = pLoad
      , nodeQLoad = qLoad
      , nodePMax = solarCap * effectiveSolar  -- Available now (not capacity)
      , nodeQMax = qMax
      , nodeSRated = sRated
      })

  -- Edge parameters
  edgeParams <- forM edges $ \e@(NodeId i, NodeId j) -> do
    let dist = fromIntegral (abs (i - j)) * 20
        r = 0.001 * dist
        x = 0.0005 * dist
    return (e, EdgeParams r x 50 dist)

  return (ACProblem
    { acNodes = nodes
    , acSlackNode = NodeId 0
    , acNodeState = Map.fromList nodeStates
    , acEdges = edges
    , acEdgeParams = Map.fromList edgeParams
    , acVMin = 160
    , acVMax = 270
    , acFreqNominal = 50
    , acFreqMin = 49.5
    , acFreqMax = 50.5
    , acCurtailPenalty = 10.0  -- Prefer not to curtail
    }, hour)

-- | Generate realistic dataset for neural network training
--
-- CSV columns: hour, pgen_available_0..N, pload_0..N, qload_0..N,
--              p_setpoint_0..N, q_setpoint_0..N, losses, curtailed_total
--
-- Recommended: 100k samples for good coverage
generateRealisticDataset :: Int       -- ^ Number of samples
                         -> Int       -- ^ Number of nodes
                         -> Int       -- ^ Connectivity factor
                         -> FilePath  -- ^ Output CSV path
                         -> IO Int    -- ^ Number of feasible samples
generateRealisticDataset nSamples nNodes k outPath = do
  putStrLn $ printf "Generating %d samples with %d nodes (realistic profiles)..." nSamples nNodes

  samples <- fmap catMaybes $ replicateM nSamples $ do
    (prob, hour) <- sampleFromProfiles nNodes k defaultSolarProfile defaultLoadProfile
    sol <- solveACOPF prob
    return $ case sol of
      Just s | solFeasible s -> Just (prob, s, hour)
      _ -> Nothing

  putStrLn $ printf "Got %d feasible samples" (length samples)

  -- Build CSV with P,Q setpoints as targets
  let pvNodes = [i | i <- [0..nNodes-1], i `mod` 3 /= 2, i /= 0]  -- PV bus indices

      header = "hour," ++
               intercalate "," ["pmax_" ++ show i | i <- pvNodes] ++ "," ++
               intercalate "," ["pload_" ++ show i | i <- [0..nNodes-1]] ++ "," ++
               intercalate "," ["qload_" ++ show i | i <- [0..nNodes-1]] ++ "," ++
               intercalate "," ["p_set_" ++ show i | i <- pvNodes] ++ "," ++
               intercalate "," ["q_set_" ++ show i | i <- pvNodes] ++ "," ++
               "losses,curtailed"

      rows = map sampleToRow samples

      sampleToRow (prob, sol, hour) =
        let nodeMap = acNodeState prob
            dispatch = solDispatch sol
            pSets = odPSetpoints dispatch
            qSets = odQSetpoints dispatch
            curtailed = sum $ Map.elems $ odCurtailed dispatch
        in printf "%.2f," hour ++
           intercalate "," [printf "%.4f" (nodePMax $ nodeMap Map.! NodeId i) | i <- pvNodes] ++ "," ++
           intercalate "," [printf "%.4f" (nodePLoad $ nodeMap Map.! NodeId i) | i <- [0..nNodes-1]] ++ "," ++
           intercalate "," [printf "%.4f" (nodeQLoad $ nodeMap Map.! NodeId i) | i <- [0..nNodes-1]] ++ "," ++
           intercalate "," [printf "%.4f" (Map.findWithDefault 0 (show i) pSets) | i <- pvNodes] ++ "," ++
           intercalate "," [printf "%.4f" (Map.findWithDefault 0 (show i) qSets) | i <- pvNodes] ++ "," ++
           printf "%.4f,%.4f" (solLosses sol) curtailed

  writeFile outPath $ unlines (header : rows)
  return (length samples)

#else

-- GHCJS stub
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

newtype NodeId = NodeId Int deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON, Num)
data NodeType = SlackBus | PVBus | PQBus deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)
data NodeState = NodeState
  { nodeType :: NodeType, nodeV :: Double, nodeDelta :: Double
  , nodePGen :: Double, nodeQGen :: Double, nodePLoad :: Double, nodeQLoad :: Double
  , nodePMax :: Double, nodeQMax :: Double, nodeSRated :: Double
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)
data EdgeParams = EdgeParams
  { edgeR :: Double, edgeX :: Double, edgeCap :: Double, edgeLength :: Double
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)
data ACProblem = ACProblem
  { acNodes :: [NodeId], acSlackNode :: NodeId, acNodeState :: Map NodeId NodeState
  , acEdges :: [(NodeId, NodeId)], acEdgeParams :: Map (NodeId, NodeId) EdgeParams
  , acVMin :: Double, acVMax :: Double, acFreqNominal :: Double
  , acFreqMin :: Double, acFreqMax :: Double, acCurtailPenalty :: Double
  } deriving (Show, Generic, NFData, ToJSON, FromJSON)
data OptimalDispatch = OptimalDispatch
  { odPSetpoints :: Map NodeId Double, odQSetpoints :: Map NodeId Double
  , odCurtailed :: Map NodeId Double
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)
data ACSolution = ACSolution
  { solVoltages :: Map NodeId (Double, Double), solPFlows :: Map (NodeId, NodeId) Double
  , solQFlows :: Map (NodeId, NodeId) Double, solLosses :: Double
  , solDispatch :: OptimalDispatch, solObjective :: Double, solFeasible :: Bool, solIterations :: Int
  } deriving (Show, Generic, NFData, ToJSON, FromJSON)
newtype SolarProfile = SolarProfile { unSolarProfile :: [Double] } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)
newtype LoadProfile = LoadProfile { unLoadProfile :: [Double] } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

defaultSolarProfile :: SolarProfile
defaultSolarProfile = SolarProfile []
defaultLoadProfile :: LoadProfile
defaultLoadProfile = LoadProfile []
sampleTimeOfDay :: IO Double
sampleTimeOfDay = return 12.0
solveACOPF :: ACProblem -> IO (Maybe ACSolution)
solveACOPF _ = return Nothing
sampleFromProfiles :: Int -> Int -> SolarProfile -> LoadProfile -> IO (ACProblem, Double)
sampleFromProfiles _ _ _ _ = error "Not available in GHCJS"
generateRealisticDataset :: Int -> Int -> Int -> FilePath -> IO Int
generateRealisticDataset _ _ _ _ = return 0

#endif
