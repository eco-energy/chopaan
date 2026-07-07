{-# LANGUAGE OverloadedStrings #-}

-- | Feeders come from mgenv's *real* @Grid.Sample.generateGrid@.
--
-- Why a subprocess and not a direct import: mgenv is pinned to GHC 8.6.5
-- (ConCat / hgeometry / streamly / monad-bayes), while dear-imgui needs GHC
-- ≥8.10 (we build on 9.0.1). One program cannot link both GHCs, so the renderer
-- crosses the boundary by *running mgenv's own binary* (`mgenv-json`) and
-- decoding its output — i.e. it invokes the actual mgenv sampler live, rather
-- than baking a static file. Set @MGENV_JSON@ to the binary, or drop it on PATH.
-- Falls back to a bundled JSON, then a built-in demo, if mgenv isn't reachable.
module Feeder
  ( Feeder(..)
  , sampleFeeders     -- live: run mgenv's generateGrid via its binary
  , loadFeedersFile   -- fallback: decode a committed grids.json
  , demoFeeder
  ) where

import           Control.Exception (try, SomeException)
import           Data.Aeson (FromJSON(..), withObject, eitherDecode, (.:))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           System.Directory (findExecutable)
import           System.Environment (lookupEnv)
import           System.Process (readProcess)

data Feeder = Feeder
  { fdEdges :: [(Int,Int)]
  , fdCond  :: [Double]
  , fdRated :: [Double]
  , fdLoad  :: [Double]
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
    pure Feeder { fdEdges = zip src tgt, fdCond = map (/ m) c0
                , fdRated = rated, fdLoad = load }

newtype Grids = Grids [Feeder]
instance FromJSON Grids where
  parseJSON = withObject "grids" $ \o -> Grids <$> o .: "grids"

decodeGrids :: BSL.ByteString -> [Feeder]
decodeGrids b = case eitherDecode b of Right (Grids gs) -> gs; Left _ -> []

-- | Sample @n@ fresh feeders by invoking mgenv's real generateGrid (its binary
-- writes the grids JSON to stdout when given no output path). Locates the binary
-- via $MGENV_JSON or PATH; on any failure returns [] so the caller can fall back.
sampleFeeders :: Int -> IO [Feeder]
sampleFeeders n = do
  mbin <- lookupEnv "MGENV_JSON"
  bin  <- maybe (findExecutable "mgenv-json") (pure . Just) mbin
  case bin of
    Nothing -> pure []
    Just b  -> do
      r <- try (readProcess b [show n] "") :: IO (Either SomeException String)
      pure $ either (const []) (decodeGrids . BSLC.pack) r

-- | Fallback: decode a committed grids.json.
loadFeedersFile :: FilePath -> IO [Feeder]
loadFeedersFile fp = do
  e <- try (BSL.readFile fp) :: IO (Either SomeException BSL.ByteString)
  pure $ either (const []) decodeGrids e

demoFeeder :: Feeder
demoFeeder = Feeder
  { fdEdges = [(0,1),(1,2),(1,3)]
  , fdCond  = [1.0, 0.6, 0.6]
  , fdRated = [0.1, 0.5, 0.0, 0.0]
  , fdLoad  = [0.0, 0.3, 0.6, 0.5]
  }
