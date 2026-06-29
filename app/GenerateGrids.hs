{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Generate a JSON manifest of sampled distribution grids using mgenv.
--
-- Output schema (one entry per sampled grid):
--   { "grids": [{ "num_nodes": Int
--               , "num_edges": Int
--               , "edge_src":          [Int]    -- length E
--               , "edge_tgt":          [Int]    -- length E
--               , "edge_r_pu":         [Double] -- per-unit on (V_base, S_base)
--               , "node_type":         [String] -- "slack" | "pv" | "pq"
--               , "node_p_rated_kw":   [Double]
--               , "node_p_load_kw":    [Double]
--               , "node_q_load_kvar":  [Double]
--               , "v_base_v":          Double
--               , "s_base_kw":         Double
--               }, ...] }
--
-- The C/Python vec env loads this manifest and runs Hopfield settling per env
-- over the per-grid topology, so each parallel env sees a different sampled
-- distribution feeder.
--
-- Usage:
--   chopaan-gen-grids --num-grids 64 --output grids.json

module Main (main) where

import qualified Algebra.Graph.Labelled as LG
import qualified Algebra.Graph.ToGraph as TG
import Control.Monad (replicateM)
import Control.Monad.Bayes.Sampler (sampleIO)
import Data.Aeson (ToJSON(..), encode, object, (.=))
import qualified Data.ByteString.Lazy as BSL
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import System.Environment (getArgs)

import qualified Grid.HH as HH
import Grid.Sample (SampledGrid(..), generateGrid, sampleGridSpec)
import qualified Physics.Consumption as Cn
import qualified Physics.PV as PV
import qualified Physics.Transmission as Tx

-- Per-unit base for Pakistan low-voltage distribution.
vBaseV, sBaseKW :: Double
vBaseV  = 220.0
sBaseKW = 100.0

-- | Resistance in ohms from a TransmissionSpec (R = ρ·L/A).
edgeResistance :: Tx.TransmissionSpec -> Double
edgeResistance s =
  realToFrac (Tx.resistivity s) * realToFrac (Tx.wireLength s)
    / max 1e-12 (realToFrac (Tx.crossSection s))

-- | Per-unit resistance on (V_base, S_base): R_pu = R_ohm · S_base / V_base².
perUnit :: Double -> Double
perUnit rOhm = rOhm * (sBaseKW * 1000.0) / (vBaseV * vBaseV)

pvRatedKW :: PV.PVSpec -> Double
pvRatedKW pv = realToFrac (PV.power pv) / 1000.0

loadKW :: Cn.Load -> Double
loadKW l = realToFrac (Cn.power l) / 1000.0

hhLoadKW :: HH.HHSpec -> Double
hhLoadKW hh =
  let Cn.ConsumptionSpec loads = HH.consumption hh
  in sum (map loadKW loads)

data NodeKind = Slack | PV | PQ deriving (Eq, Show)

kindStr :: NodeKind -> String
kindStr Slack = "slack"
kindStr PV    = "pv"
kindStr PQ    = "pq"

classify :: HH.NodeId -> HH.HHSpec -> NodeKind
classify slackId hh
  | HH.nId hh == slackId          = Slack
  | pvRatedKW (HH.generation hh) > 0 = PV
  | otherwise                        = PQ

data GridJSON = GridJSON
  { numNodes      :: !Int
  , numEdges      :: !Int
  , edgeSrc       :: ![Int]
  , edgeTgt       :: ![Int]
  , edgeRPu       :: ![Double]
  , nodeType      :: ![String]
  , nodePRatedKW  :: ![Double]
  , nodePLoadKW   :: ![Double]
  , nodeQLoadKVAr :: ![Double]
  }

instance ToJSON GridJSON where
  toJSON g = object
    [ "num_nodes"        .= numNodes g
    , "num_edges"        .= numEdges g
    , "edge_src"         .= edgeSrc g
    , "edge_tgt"         .= edgeTgt g
    , "edge_r_pu"        .= edgeRPu g
    , "node_type"        .= nodeType g
    , "node_p_rated_kw"  .= nodePRatedKW g
    , "node_p_load_kw"   .= nodePLoadKW g
    , "node_q_load_kvar" .= nodeQLoadKVAr g
    , "v_base_v"         .= vBaseV
    , "s_base_kw"        .= sBaseKW
    ]

gridToJSON :: SampledGrid -> Maybe GridJSON
gridToJSON (SampledGrid g) =
  case sortBy (comparing HH.nId) (TG.vertexList g) of
    [] -> Nothing
    nodes@(slack:_) ->
      let slackId  = HH.nId slack
          idxOf    = Map.fromList (zip (map HH.nId nodes) [0 :: Int ..])
          n        = length nodes
          edgesRaw = [(s, t, lbl)
                     | (lbl, s, t) <- LG.edgeList g
                     , HH.nId s /= HH.nId t
                     ]
          es       = [idxOf Map.! HH.nId s | (s, _, _) <- edgesRaw]
          ts       = [idxOf Map.! HH.nId t | (_, t, _) <- edgesRaw]
          rs       = [perUnit (edgeResistance lbl) | (_, _, lbl) <- edgesRaw]
          kinds    = map (kindStr . classify slackId) nodes
          rated    = map (pvRatedKW . HH.generation) nodes
          ploads   = map hhLoadKW nodes
          qloads   = map (0.3 *) ploads
      in Just GridJSON
           { numNodes      = n
           , numEdges      = length edgesRaw
           , edgeSrc       = es
           , edgeTgt       = ts
           , edgeRPu       = rs
           , nodeType      = kinds
           , nodePRatedKW  = rated
           , nodePLoadKW   = ploads
           , nodeQLoadKVAr = qloads
           }

parseArgs :: [String] -> (Int, FilePath)
parseArgs = go 16 "grids.json"
  where
    go !k !o []                       = (k, o)
    go _  o  ("--num-grids":v:rest)   = go (read v) o rest
    go k  _  ("--output":v:rest)      = go k v rest
    go k  o  (_:rest)                 = go k o rest

main :: IO ()
main = do
  (numGrids, outPath) <- parseArgs <$> getArgs
  putStrLn $ "Sampling " ++ show numGrids ++ " grids -> " ++ outPath
  grids <- sampleIO $ replicateM numGrids $ do
    spec <- sampleGridSpec
    generateGrid spec
  let entries = [g | Just g <- map gridToJSON grids]
      doc     = object ["grids" .= entries]
  BSL.writeFile outPath (encode doc)
  putStrLn $ "Wrote " ++ show (length entries) ++ " grids to " ++ outPath
