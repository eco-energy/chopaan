{-# LANGUAGE OverloadedStrings #-}

-- | Load the real mgenv feeders from @grids_4node.json@ (the schema
-- @mgenv-json@ emits) into a compact record the renderer/kernel consume.
module Feeder
  ( Feeder(..)
  , loadFeeders
  , demoFeeder
  ) where

import           Control.Exception (try, SomeException)
import           Data.Aeson (FromJSON(..), withObject, eitherDecode, (.:))
import qualified Data.ByteString.Lazy as BSL

data Feeder = Feeder
  { fdEdges :: [(Int,Int)]   -- directed edges (src,tgt), dense node ids 0..3
  , fdCond  :: [Double]      -- per-edge conductance 1/R, normalized to max=1
  , fdRated :: [Double]      -- per-node PV rating (kW)
  , fdLoad  :: [Double]      -- per-node base real load (kW)
  } deriving Show

instance FromJSON Feeder where
  parseJSON = withObject "grid" $ \o -> do
    src   <- o .: "edge_src"
    tgt   <- o .: "edge_tgt"
    rohm  <- o .: "edge_r_ohm"
    rated <- o .: "node_p_rated_kw"
    load  <- o .: "node_p_load_kw"
    let c0 = map (\x -> 1 / max 1e-9 x) (rohm :: [Double])
        m  = if null c0 then 1 else maximum c0
    pure Feeder { fdEdges = zip src tgt
                , fdCond  = map (/ m) c0
                , fdRated = rated
                , fdLoad  = load }

newtype Grids = Grids [Feeder]
instance FromJSON Grids where
  parseJSON = withObject "grids" $ \o -> Grids <$> o .: "grids"

-- | Best-effort load; returns [] on any error so the app can fall back.
loadFeeders :: FilePath -> IO [Feeder]
loadFeeders fp = do
  e <- try (BSL.readFile fp) :: IO (Either SomeException BSL.ByteString)
  pure $ case e of
    Left _  -> []
    Right b -> case eitherDecode b of
      Right (Grids gs) -> gs
      Left _           -> []

-- | Fallback 4-node radial feeder if no JSON is found.
demoFeeder :: Feeder
demoFeeder = Feeder
  { fdEdges = [(0,1),(1,2),(1,3)]
  , fdCond  = [1.0, 0.6, 0.6]
  , fdRated = [0.1, 0.5, 0.0, 0.0]
  , fdLoad  = [0.0, 0.3, 0.6, 0.5]
  }
