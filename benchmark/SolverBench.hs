{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Criterion.Main
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.SBV
import Data.SBV.Control
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Control.Monad (forM_)

-- | Node identifier
newtype NodeId = NodeId Int
  deriving (Eq, Ord, Show, Generic, NFData)

-- | Transport problem specification
data TransportProblem = TransportProblem
  { sources :: [(NodeId, Double)]           -- (node, supply capacity)
  , sinks   :: [(NodeId, Double)]           -- (node, demand)
  , costs   :: Map (NodeId, NodeId) Double  -- edge costs
  } deriving (Show, Generic, NFData)

-- | Generate a balanced grid topology
-- n sources, n sinks, full bipartite connectivity
mkProblem :: Int -> TransportProblem
mkProblem n = TransportProblem
  { sources = [(NodeId i, 10.0) | i <- [0..n-1]]
  , sinks   = [(NodeId i, 10.0) | i <- [n..2*n-1]]
  , costs   = Map.fromList
      [ ((NodeId i, NodeId j), fromIntegral (abs (i - j) + 1))
      | i <- [0..n-1]
      , j <- [n..2*n-1]
      ]
  }

-- | Generate a sparse mesh topology (more realistic)
-- Each node connects to at most k neighbors
mkSparseProblem :: Int -> Int -> TransportProblem
mkSparseProblem n k = TransportProblem
  { sources = [(NodeId i, 10.0) | i <- [0..n-1], i `mod` 3 == 0]
  , sinks   = [(NodeId i, 10.0) | i <- [0..n-1], i `mod` 3 /= 0]
  , costs   = Map.fromList
      [ ((NodeId i, NodeId j), fromIntegral (abs (i - j) + 1))
      | i <- [0..n-1]
      , j <- [0..n-1]
      , i /= j
      , abs (i - j) <= k  -- only connect to k nearest neighbors
      ]
  }

-- | Solve transport problem, return Maybe total cost
solveTransport :: TransportProblem -> IO (Maybe Double)
solveTransport problem = runSMT $ do
  -- Create flow variables for each edge
  let edges = Map.keys (costs problem)
  flows <- mapM mkFlow edges
  let flowMap = Map.fromList (zip edges flows)

  -- Non-negativity
  mapM_ (\f -> constrain $ f .>= 0) flows

  -- Source conservation: outflow = supply
  forM_ (sources problem) $ \(node, supply) -> do
    let outflow = sum [flowMap Map.! (node, j) | (i, j) <- edges, i == node]
    constrain $ outflow .== literal supply

  -- Sink conservation: inflow = demand
  forM_ (sinks problem) $ \(node, demand) -> do
    let inflow = sum [flowMap Map.! (i, node) | (i, j) <- edges, j == node]
    constrain $ inflow .== literal demand

  -- Minimize cost
  let totalCost = sum
        [ literal (costs problem Map.! e) * f
        | (e, f) <- zip edges flows
        ]
  minimize "cost" totalCost

  query $ do
    cs <- checkSat
    case cs of
      Sat -> Just <$> getValue totalCost
      _   -> return Nothing
  where
    mkFlow (NodeId i, NodeId j) =
      sDouble ("f_" ++ show i ++ "_" ++ show j)

-- | Benchmarks
main :: IO ()
main = defaultMain
  [ bgroup "bipartite"
    [ bench "4 nodes"   $ nfIO (solveTransport (mkProblem 2))
    , bench "8 nodes"   $ nfIO (solveTransport (mkProblem 4))
    , bench "16 nodes"  $ nfIO (solveTransport (mkProblem 8))
    , bench "32 nodes"  $ nfIO (solveTransport (mkProblem 16))
    , bench "64 nodes"  $ nfIO (solveTransport (mkProblem 32))
    , bench "128 nodes" $ nfIO (solveTransport (mkProblem 64))
    , bench "256 nodes" $ nfIO (solveTransport (mkProblem 128))
    ]
  , bgroup "sparse-mesh-k4"
    [ bench "12 nodes"  $ nfIO (solveTransport (mkSparseProblem 12 4))
    , bench "24 nodes"  $ nfIO (solveTransport (mkSparseProblem 24 4))
    , bench "48 nodes"  $ nfIO (solveTransport (mkSparseProblem 48 4))
    , bench "96 nodes"  $ nfIO (solveTransport (mkSparseProblem 96 4))
    , bench "192 nodes" $ nfIO (solveTransport (mkSparseProblem 192 4))
    , bench "384 nodes" $ nfIO (solveTransport (mkSparseProblem 384 4))
    ]
  , bgroup "sparse-mesh-k8"
    [ bench "12 nodes"  $ nfIO (solveTransport (mkSparseProblem 12 8))
    , bench "24 nodes"  $ nfIO (solveTransport (mkSparseProblem 24 8))
    , bench "48 nodes"  $ nfIO (solveTransport (mkSparseProblem 48 8))
    , bench "96 nodes"  $ nfIO (solveTransport (mkSparseProblem 96 8))
    , bench "192 nodes" $ nfIO (solveTransport (mkSparseProblem 192 8))
    ]
  ]
