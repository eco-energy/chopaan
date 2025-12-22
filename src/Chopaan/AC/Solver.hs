{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

-- | AC Power Flow Solver using SBV/dReal
--
-- Full AC power flow for grid-tied solar at 220V nominal (160-270V Pakistan range),
-- WAPDA sync, phase coupling. Uses SBV with dReal for exact nonlinear solving.
--
-- For each edge (i,j) with impedance Z_ij = R_ij + jX_ij:
--
-- @
-- P_ij = (V_i² × G_ij) - (V_i × V_j × (G_ij × cos(δ_i - δ_j) + B_ij × sin(δ_i - δ_j)))
-- Q_ij = -(V_i² × B_ij) - (V_i × V_j × (G_ij × sin(δ_i - δ_j) - B_ij × cos(δ_i - δ_j)))
-- @
--
-- Where:
--   * V_i, V_j = voltage magnitudes
--   * δ_i, δ_j = voltage angles
--   * G_ij = R_ij / |Z_ij|² (conductance)
--   * B_ij = -X_ij / |Z_ij|² (susceptance)

module Chopaan.AC.Solver
  ( -- * Types
    NodeId(..)
  , NodeState(..)
  , EdgeParams(..)
  , ACProblem(..)
  , ACSolution(..)
    -- * Solver
  , solveACPowerFlow
    -- * Dataset Generation
  , randomProblem
  , generateSample
  , generateDataset
  ) where

#ifndef ghcjs_HOST_OS

import Data.SBV
import Data.SBV.Trans.Control
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad (forM, replicateM, when)
import System.Random (randomRIO, newStdGen)
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON)

-- | Node identifier in the network
newtype NodeId = NodeId Int
  deriving (Eq, Ord, Show, Generic, NFData, ToJSON, FromJSON)

-- | Node electrical state
data NodeState = NodeState
  { nodeV     :: Double    -- ^ Voltage magnitude (V), nominal 220, range 160-270
  , nodeDelta :: Double    -- ^ Voltage angle (radians)
  , nodePGen  :: Double    -- ^ Real power generation (kW)
  , nodeQGen  :: Double    -- ^ Reactive power generation (kVAR)
  , nodePLoad :: Double    -- ^ Real power load (kW)
  , nodeQLoad :: Double    -- ^ Reactive power load (kVAR)
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

-- | Edge parameters (transmission line characteristics)
data EdgeParams = EdgeParams
  { edgeR   :: Double      -- ^ Resistance (Ω)
  , edgeX   :: Double      -- ^ Reactance (Ω)
  , edgeCap :: Double      -- ^ Thermal capacity (kVA)
  } deriving (Show, Eq, Generic, NFData, ToJSON, FromJSON)

-- | Full AC power flow problem specification
data ACProblem = ACProblem
  { acNodes      :: [NodeId]
  , acSlackNode  :: NodeId                         -- ^ WAPDA connection point (fixed V, δ=0)
  , acNodeState  :: Map NodeId NodeState
  , acEdges      :: [(NodeId, NodeId)]
  , acEdgeParams :: Map (NodeId, NodeId) EdgeParams
  , acVMin       :: Double                         -- ^ Minimum voltage (160V)
  , acVMax       :: Double                         -- ^ Maximum voltage (270V)
  } deriving (Show, Generic, NFData, ToJSON, FromJSON)

-- | Solution to AC power flow problem
data ACSolution = ACSolution
  { solVoltages :: Map NodeId (Double, Double)     -- ^ (V, δ) for each node
  , solPFlows   :: Map (NodeId, NodeId) Double     -- ^ Real power flow per edge
  , solQFlows   :: Map (NodeId, NodeId) Double     -- ^ Reactive power flow per edge
  , solLosses   :: Double                          -- ^ Total losses
  , solFeasible :: Bool
  } deriving (Show, Generic, NFData, ToJSON, FromJSON)

-- | dReal configuration for nonlinear solving
dRealConfig :: SMTConfig
dRealConfig = defaultSMTCfg
  { solver = dReal
  , verbose = False
  }

-- | Solve AC power flow using dReal nonlinear solver
--
-- Expected runtime: ~10s per solve for typical residential networks
solveACPowerFlow :: ACProblem -> IO (Maybe ACSolution)
solveACPowerFlow ACProblem{..} = runSMTWith dRealConfig $ do

  -- Create voltage magnitude variables (except slack)
  vVars <- forM acNodes $ \n@(NodeId i) -> do
    v <- sReal ("V_" ++ show i)
    if n == acSlackNode
      then constrain $ v .== literal 220  -- slack bus fixed at nominal
      else do
        constrain $ v .>= literal acVMin
        constrain $ v .<= literal acVMax
    return (n, v)
  let vMap = Map.fromList vVars

  -- Create voltage angle variables (slack = 0)
  dVars <- forM acNodes $ \n@(NodeId i) -> do
    d <- sReal ("delta_" ++ show i)
    if n == acSlackNode
      then constrain $ d .== 0  -- slack bus reference angle
      else do
        constrain $ d .>= literal (-pi/6)  -- ±30° typical limit
        constrain $ d .<= literal (pi/6)
    return (n, d)
  let dMap = Map.fromList dVars

  -- Power flow equations for each edge
  pFlows <- forM acEdges $ \e@(ni, nj) -> do
    let EdgeParams r x _ = acEdgeParams Map.! e
        zMag2 = r*r + x*x
        g = r / zMag2           -- conductance
        b = -x / zMag2          -- susceptance
        vi = vMap Map.! ni
        vj = vMap Map.! nj
        di = dMap Map.! ni
        dj = dMap Map.! nj
        dij = di - dj

    -- P_ij = Vi² × G - Vi × Vj × (G×cos(δij) + B×sin(δij))
    let pij = (vi * vi * literal g)
            - (vi * vj * (literal g * cos dij + literal b * sin dij))

    -- Q_ij = -Vi² × B - Vi × Vj × (G×sin(δij) - B×cos(δij))
    let qij = -(vi * vi * literal b)
            - (vi * vj * (literal g * sin dij - literal b * cos dij))

    return (e, (pij, qij))

  let pFlowMap = Map.fromList [(e, p) | (e, (p, _)) <- pFlows]
      qFlowMap = Map.fromList [(e, q) | (e, (_, q)) <- pFlows]

  -- Node power balance constraints (except slack which absorbs imbalance)
  forM_ acNodes $ \n -> do
    when (n /= acSlackNode) $ do
      let NodeState{..} = acNodeState Map.! n
          pNet = literal (nodePGen - nodePLoad)
          qNet = literal (nodeQGen - nodeQLoad)

          -- Sum of outflows - inflows
          pOut = sum [pFlowMap Map.! (n, j) | (i, j) <- acEdges, i == n]
          pIn  = sum [pFlowMap Map.! (i, n) | (i, j) <- acEdges, j == n]
          qOut = sum [qFlowMap Map.! (n, j) | (i, j) <- acEdges, i == n]
          qIn  = sum [qFlowMap Map.! (i, n) | (i, j) <- acEdges, j == n]

      -- Power balance: P_gen - P_load = P_out - P_in
      constrain $ pNet .== pOut - pIn
      -- Reactive power balance
      constrain $ qNet .== qOut - qIn

  -- Solve with dReal
  query $ do
    cs <- checkSat
    case cs of
      Sat -> do
        voltages <- forM acNodes $ \n -> do
          v <- getValue (vMap Map.! n)
          d <- getValue (dMap Map.! n)
          return (n, (v, d))

        pFlowVals <- forM acEdges $ \e -> do
          p <- getValue (pFlowMap Map.! e)
          return (e, p)

        qFlowVals <- forM acEdges $ \e -> do
          q <- getValue (qFlowMap Map.! e)
          return (e, q)

        -- Calculate total losses (sum of absolute power flows)
        let losses = sum [abs p | (_, p) <- pFlowVals]

        return $ Just ACSolution
          { solVoltages = Map.fromList voltages
          , solPFlows = Map.fromList pFlowVals
          , solQFlows = Map.fromList qFlowVals
          , solLosses = losses
          , solFeasible = True
          }

      _ -> return Nothing

-- | Generate a random problem instance for training data
randomProblem :: Int   -- ^ Number of nodes
              -> Int   -- ^ Connectivity factor (edges to neighbors within k hops)
              -> IO ACProblem
randomProblem nNodes k = do
  _ <- newStdGen
  let nodes = [NodeId i | i <- [0..nNodes-1]]
      -- Create edges between nodes within k hops (sparse connectivity)
      edges = [(NodeId i, NodeId j) | i <- [0..nNodes-1], j <- [0..nNodes-1],
               i /= j, abs(i-j) <= k]

  -- Random node states (typical residential solar + load)
  nodeStates <- forM nodes $ \n -> do
    pGen  <- randomRIO (0, 10)      -- 0-10 kW solar generation
    pLoad <- randomRIO (0.5, 5)     -- 0.5-5 kW load
    qLoad <- randomRIO (0, 1)       -- 0-1 kVAR reactive load
    return (n, NodeState 220 0 pGen 0 pLoad qLoad)

  -- Edge parameters (typical residential cable impedances)
  edgeParams <- forM edges $ \e@(NodeId i, NodeId j) -> do
    let dist = fromIntegral (abs (i - j)) * 20  -- ~20m per hop
        r = 0.001 * dist   -- ~1mΩ/m for typical cable
        x = 0.0005 * dist  -- ~0.5mΩ/m reactive
    return (e, EdgeParams r x 50)  -- 50 kVA thermal capacity

  return ACProblem
    { acNodes = nodes
    , acSlackNode = NodeId 0  -- First node is WAPDA connection
    , acNodeState = Map.fromList nodeStates
    , acEdges = edges
    , acEdgeParams = Map.fromList edgeParams
    , acVMin = 160
    , acVMax = 270
    }

-- | Generate a single (input, output) sample for training
generateSample :: Int -> Int -> IO (Maybe (ACProblem, ACSolution))
generateSample nNodes k = do
  prob <- randomProblem nNodes k
  sol <- solveACPowerFlow prob
  return $ (prob,) <$> sol

-- | Generate dataset for neural network training
--
-- Writes CSV with columns:
--   sample_id, pgen_0..pgen_N, pload_0..pload_N, v_0..v_N, delta_0..delta_N
--
-- Expected runtime: ~10s per sample, so 10,000 samples ≈ 28 hours
generateDataset :: Int       -- ^ Number of samples
                -> Int       -- ^ Number of nodes
                -> Int       -- ^ Connectivity factor
                -> FilePath  -- ^ Output CSV path
                -> IO Int    -- ^ Number of feasible samples generated
generateDataset nSamples nNodes k outPath = do
  putStrLn $ "Generating " ++ show nSamples ++ " samples with " ++ show nNodes ++ " nodes..."
  samples <- catMaybes <$> replicateM nSamples (generateSample nNodes k)
  putStrLn $ "Got " ++ show (length samples) ++ " feasible samples"

  -- Build CSV
  let header = "sample_id," ++ inputCols ++ "," ++ outputCols
      inputCols = intercalate "," ["pgen_" ++ show i | i <- [0..nNodes-1]] ++ "," ++
                  intercalate "," ["pload_" ++ show i | i <- [0..nNodes-1]]
      outputCols = intercalate "," ["v_" ++ show i | i <- [0..nNodes-1]] ++ "," ++
                   intercalate "," ["delta_" ++ show i | i <- [0..nNodes-1]]

      rows = zipWith sampleToRow [0..] samples

  writeFile outPath $ unlines (header : rows)
  return (length samples)
  where
    sampleToRow :: Int -> (ACProblem, ACSolution) -> String
    sampleToRow idx (prob, sol) =
      show idx ++ "," ++
      intercalate "," [show (nodePGen s) | (_, s) <- Map.toList (acNodeState prob)] ++ "," ++
      intercalate "," [show (nodePLoad s) | (_, s) <- Map.toList (acNodeState prob)] ++ "," ++
      intercalate "," [show v | (_, (v, _)) <- Map.toList (solVoltages sol)] ++ "," ++
      intercalate "," [show d | (_, (_, d)) <- Map.toList (solVoltages sol)]

#else

-- GHCJS stub - AC solver requires native SBV/dReal
data NodeId = NodeId Int deriving (Eq, Ord, Show)
data NodeState = NodeState deriving (Show, Eq)
data EdgeParams = EdgeParams deriving (Show, Eq)
data ACProblem = ACProblem deriving (Show)
data ACSolution = ACSolution deriving (Show)

solveACPowerFlow :: ACProblem -> IO (Maybe ACSolution)
solveACPowerFlow _ = pure Nothing

randomProblem :: Int -> Int -> IO ACProblem
randomProblem _ _ = error "AC solver not available in GHCJS"

generateSample :: Int -> Int -> IO (Maybe (ACProblem, ACSolution))
generateSample _ _ = pure Nothing

generateDataset :: Int -> Int -> Int -> FilePath -> IO Int
generateDataset _ _ _ _ = pure 0

#endif
