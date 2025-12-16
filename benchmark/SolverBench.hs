{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Benchmark for Chopaan's transport problem solver
-- This is the exact algorithm from Chopaan.Kibbutz.LinOpt.transportProblem
module Main where

import Criterion.Main
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.SBV
import Data.List (transpose)

--------------------------------------------------------------------------------
-- Types from Chopaan.Kibbutz.LinOpt (exact copy)
--------------------------------------------------------------------------------

newtype Sources n = Sources { unSource :: [(n, Double)] }
  deriving (Eq, Ord, Show, Generic, NFData)

newtype Sinks n = Sinks { unSink :: [(n, Double)] }
  deriving (Eq, Ord, Show, Generic, NFData)

instance Semigroup (Sources n) where
  (Sources a) <> (Sources b) = Sources (a <> b)

instance Semigroup (Sinks n) where
  (Sinks a) <> (Sinks b) = Sinks (a <> b)

instance Monoid (Sources n) where
  mempty = Sources []

instance Monoid (Sinks n) where
  mempty = Sinks []

class NamedF a n where
  getVals :: a n -> [Double]
  getNames :: a n -> [n]

instance (Show n) => NamedF Sources n where
  getVals = (snd <$>) . unSource
  getNames = (fst <$>) . unSource

instance (Show n) => NamedF Sinks n where
  getVals = (snd <$>) . unSink
  getNames = (fst <$>) . unSink

mkSources :: Show n => [n] -> [Double] -> Sources n
mkSources ns vs = Sources $ zip ns vs

mkSinks :: Show n => [n] -> [Double] -> Sinks n
mkSinks ns vs = Sinks $ zip ns vs

--------------------------------------------------------------------------------
-- transportProblem from Chopaan.Kibbutz.LinOpt (exact algorithm)
--------------------------------------------------------------------------------

-- | Variable name generator (exact copy from LinOpt)
tName :: Show a => a -> a -> String
tName i j = ("x_" <> (show i) <> "_" <> (show j))

-- | Hadamard product (exact copy from LinOpt)
hadmard :: (Num a) => [[a]] -> [[a]] -> [[a]]
hadmard as bs = fmap (\(xs, ys) -> fmap (\(x, y) -> x * y) $ zip xs ys) $ zip as bs

-- | Transport problem optimization (exact algorithm from Chopaan.Kibbutz.LinOpt)
transportProblem :: Show n => Sources n -> Sinks n -> [[Double]] -> Symbolic ()
transportProblem ss ds cs = do
  vars <- txVars
  mapM_ (\(xs, t) -> constrain $ sum xs .>= t) $ zip vars (fromDouble <$> (getVals ds))
  mapM_ (\(xs, t) -> constrain $ sum xs .<= t) $ zip (transpose vars) (fromDouble <$> (getVals ss))
  minimize "goal" $ sum $ (fmap sum) $ hadmard vars (fmap (fmap fromDouble) cs)
  where
    txVars :: Symbolic [[SReal]]
    txVars = sequence . (fmap sequence) $ [[sReal $ tName i j
                                           |i <- getNames ss]
                                          | j <- getNames ds]
    fromDouble :: Double -> SReal
    fromDouble = realToFrac

--------------------------------------------------------------------------------
-- Benchmark setup
--------------------------------------------------------------------------------

data TransportSpec = TransportSpec
  { specSources :: Sources Int
  , specSinks   :: Sinks Int
  , specCosts   :: [[Double]]
  } deriving (Show)

-- | Generate a balanced bipartite topology
mkProblem :: Int -> TransportSpec
mkProblem n = TransportSpec
  { specSources = mkSources [1..n] (replicate n 10.0)
  , specSinks   = mkSinks [n+1..2*n] (replicate n 10.0)
  , specCosts   = [[fromIntegral (abs (i - j) + 1) | i <- [1..n]] | j <- [n+1..2*n]]
  }

-- | Generate a sparse mesh topology
mkSparseProblem :: Int -> Int -> TransportSpec
mkSparseProblem n k = TransportSpec
  { specSources = mkSources sourceIds (replicate numSources 10.0)
  , specSinks   = mkSinks sinkIds (replicate numSinks 10.0)
  , specCosts   = [[if abs (i - j) <= k then fromIntegral (abs (i - j) + 1) else 1e9
                   | i <- sourceIds] | j <- sinkIds]
  }
  where
    sourceIds = [i | i <- [1..n], i `mod` 3 == 1]
    sinkIds   = [i | i <- [1..n], i `mod` 3 /= 1]
    numSources = length sourceIds
    numSinks   = length sinkIds

-- | Solve using the exact Chopaan algorithm
solveTransport :: TransportSpec -> IO OptimizeResult
solveTransport spec = optimize Lexicographic $
  transportProblem (specSources spec) (specSinks spec) (specCosts spec)

-- | Benchmarks using Chopaan's transport solver algorithm
main :: IO ()
main = defaultMain
  [ bgroup "chopaan-bipartite"
    [ bench "4 nodes"    $ whnfIO (solveTransport (mkProblem 2))
    , bench "8 nodes"    $ whnfIO (solveTransport (mkProblem 4))
    , bench "16 nodes"   $ whnfIO (solveTransport (mkProblem 8))
    , bench "32 nodes"   $ whnfIO (solveTransport (mkProblem 16))
    , bench "64 nodes"   $ whnfIO (solveTransport (mkProblem 32))
    , bench "128 nodes"  $ whnfIO (solveTransport (mkProblem 64))
    , bench "256 nodes"  $ whnfIO (solveTransport (mkProblem 128))
    , bench "512 nodes"  $ whnfIO (solveTransport (mkProblem 256))
    , bench "1000 nodes" $ whnfIO (solveTransport (mkProblem 500))
    ]
  ]
