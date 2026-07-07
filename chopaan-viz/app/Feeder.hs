{-# LANGUAGE ScopedTypeVariables #-}

-- | Feeders come from mgenv's real @generateGrid@, called DIRECTLY in-process.
--
-- This is possible because mgenv's generation core was migrated to GHC 9.0.1
-- (see mgenv-gen/migrate-9.0.1): EMST reimplemented with Prim's (no hgeometry),
-- dynamics/ConCat/streamly/astro stripped, monad-bayes ported. So the renderer
-- links the mgenv library and samples feeders live — no subprocess, no JSON.
module Feeder
  ( Feeder(..)
  , sampleFeeders
  , demoFeeder
  ) where

import           Control.Monad (replicateM)
import           Data.List (sortOn)
import qualified Data.Map.Strict as Map

import qualified Algebra.Graph.Labelled as LG
import           Control.Monad.Bayes.Class (normal, uniform)
import           Control.Monad.Bayes.Sampler.Strict (sampleIO)

import           Grid.Sample (GridSpec'(..), SampledGrid(..), generateGrid)
import           Grid.HH (HHSpec(..))
import qualified Physics.Consumption as Cn
import qualified Physics.PV as PV
import           Physics.Transmission (resistance)
import           Physics.Units (location)

data Feeder = Feeder
  { fdEdges :: [(Int,Int)]
  , fdCond  :: [Double]
  , fdRated :: [Double]
  , fdLoad  :: [Double]
  } deriving Show

-- | Sample @n@ feeders by running mgenv's real generateGrid in-process. The
-- feeder-size law lives here (Gaussian ~6, clamped to the kernel's [4..8]).
sampleFeeders :: Int -> IO [Feeder]
sampleFeeders n = sampleIO $ replicateM n (fmap toFeeder (generateGrid spec))
  where
    spec = GridSpec
      { geometricOrigin = pure (location 24.86 67.0)             -- Karachi
      , nNodes          = do x <- normal 6 2; pure (max 4 (min 8 (round x)))
      , nodeDist        = do m <- normal 20 60; s <- normal 10 20; normal m (abs s)
      }

-- | Flatten mgenv's labelled graph (TransmissionSpec edges, HHSpec nodes) to the
-- renderer's compact record — same reduction the JSON path used, done directly.
toFeeder :: SampledGrid -> Feeder
toFeeder (SampledGrid g) = Feeder edges cond rated load
  where
    verts   = LG.vertexList g
    nodeMap = Map.fromListWith (\_ old -> old) [ (nId hh, hh) | hh <- verts ]
    nodes   = sortOn fst (Map.toList nodeMap)
    idx     = Map.fromList (zip (map fst nodes) [0 :: Int ..])
    es      = [ (tx, s, t) | (tx, s, t) <- LG.edgeList g, nId s /= nId t ]
    edges   = [ (idx Map.! nId s, idx Map.! nId t) | (_, s, t) <- es ]
    cond0   = [ 1 / max 1e-9 (resistance tx) | (tx, _, _) <- es ]
    mx      = if null cond0 then 1 else maximum cond0
    cond    = map (/ mx) cond0
    rated   = [ PV.power (generation hh) / 1000 | (_, hh) <- nodes ]
    load    = [ let Cn.ConsumptionSpec ls = consumption hh
                in sum (map Cn.power ls) / 1000 | (_, hh) <- nodes ]

demoFeeder :: Feeder
demoFeeder = Feeder
  { fdEdges = [(0,1),(1,2),(1,3)]
  , fdCond  = [1.0, 0.6, 0.6]
  , fdRated = [0.1, 0.5, 0.0, 0.0]
  , fdLoad  = [0.0, 0.3, 0.6, 0.5]
  }
