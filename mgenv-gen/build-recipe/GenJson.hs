{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Generate distribution-grid graphs with mgenv and emit a JSON manifest.
--
-- Uses the real mgenv library:
--   sampleThis    :: SamplerIO (GridSpec' SamplerIO)   (Randomizable instance)
--   generateGrid  :: GridSpec' m -> m SampledGrid       (Grid.Sample)
--
-- SampledGrid wraps a labelled graph (TransmissionSpec edges, HHSpec nodes).
-- We flatten it to dense node indices with per-edge wire resistance and
-- per-node PV / load, then write { "grids": [...] } for the chopaan env.
--
-- This replaces the bit-rotted app/Main.hs (which calls the two-argument
-- sampleGridSpec with no arguments and does not typecheck).
--
-- Usage: mgenv-json [NUM_GRIDS] [OUTPUT_PATH]

module Main (main) where

import Control.Monad (replicateM)
import Control.Monad.Bayes.Sampler (sampleIO)
import Data.Aeson (Value, encode, object, toJSON, (.=))
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)

import qualified Algebra.Graph.Labelled as LG

import Grid.HH (HHSpec(..), NodeId)
import Grid.Sample (SampledGrid(..), GridSpec'(..), generateGrid)
import qualified Physics.Consumption as Cn
import qualified Physics.PV as PV
import Physics.Transmission (TransmissionSpec, resistance)
import Control.Monad.Bayes.Class (uniformD, normal)
import Physics.Units (location)

-- Residential feeder per-unit base (households are rooftop-scale).
vBaseV, sBaseKW :: Double
vBaseV  = 220.0
sBaseKW = 10.0

pvKW :: HHSpec -> Double
pvKW hh = PV.power (generation hh) / 1000.0

loadKW :: HHSpec -> Double
loadKW hh =
  let Cn.ConsumptionSpec loads = consumption hh
  in sum (map Cn.power loads) / 1000.0

perUnitR :: Double -> Double
perUnitR rOhm = rOhm * (sBaseKW * 1000.0) / (vBaseV * vBaseV)

-- | Flatten one SampledGrid to a JSON object.
gridToValue :: SampledGrid -> Value
gridToValue (SampledGrid g) =
  let edges     = LG.edgeList g            -- [(TransmissionSpec, HHSpec, HHSpec)]
      verts     = LG.vertexList g          -- [HHSpec] (specs repeat per nId)
      -- mgenv samples a fresh HHSpec per edge endpoint; collapse to one
      -- spec per node id (first occurrence wins).
      nodeMap   = Map.fromListWith (\_ old -> old)
                    [ (nId hh, hh) | hh <- verts ]
      nodes     = sortOn fst (Map.toList nodeMap)
      idxOf     = Map.fromList (zip (map fst nodes) [0 :: Int ..])
      ix i      = idxOf Map.! i
      keep (_, s, t) = nId s /= nId t      -- drop self-loops
      es        = filter keep edges
      edgeSrc   = [ ix (nId s) | (_, s, _) <- es ]
      edgeTgt   = [ ix (nId t) | (_, _, t) <- es ]
      edgeROhm  = [ resistance tx | (tx, _, _) <- es ]
      edgeRPu   = map perUnitR edgeROhm
      n         = length nodes
      nodeType  = "slack" : replicate (max 0 (n - 1)) ("prosumer" :: String)
      pRated    = [ pvKW hh   | (_, hh) <- nodes ]
      pLoad     = [ loadKW hh | (_, hh) <- nodes ]
      qLoad     = map (0.3 *) pLoad
  in object
       [ "num_nodes"        .= n
       , "num_edges"        .= length es
       , "edge_src"         .= edgeSrc
       , "edge_tgt"         .= edgeTgt
       , "edge_r_ohm"       .= edgeROhm
       , "edge_r_pu"        .= edgeRPu
       , "node_type"        .= nodeType
       , "node_p_rated_kw"  .= pRated
       , "node_p_load_kw"   .= pLoad
       , "node_q_load_kvar" .= qLoad
       , "v_base_v"         .= vBaseV
       , "s_base_kw"        .= sBaseKW
       ]

main :: IO ()
main = do
  args <- getArgs
  let numGrids = case args of (x:_) -> read x; _ -> 16
      outPath  = case args of (_:y:_) -> Just y; _ -> Nothing
  grids <- sampleIO $ replicateM numGrids $ do
    -- Build the GridSpec' directly. generateGrid only forces nNodes /
    -- geometricOrigin / nodeDist; startDate/rate are unused (and mgenv's
    -- Randomizable GeoC/LifeTime instances are unimplemented, so sampleThis
    -- crashes). nNodes capped small for the dispatch env.
    let spec = GridSpec
          { startDate       = undefined
          , rate            = undefined
          , geometricOrigin = pure (location 24.86 67.0)   -- Karachi
          , nNodes          = uniformD [6 .. 16]
          , nodeDist        = do { m <- normal 20 60; sd <- normal 10 20; normal m (abs sd) }
          }
    generateGrid spec
  let doc = encode (object ["grids" .= map gridToValue grids])
  case outPath of
    Just p  -> BSL.writeFile p doc >> putStrLn ("Wrote " ++ show numGrids ++ " grids to " ++ p)
    Nothing -> BSL.putStrLn doc
