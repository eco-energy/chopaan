{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

-- | Feeder-graph generation core, ported to GHC 9.0.1.
--
-- Only the sampling path survives: sample N radial households, connect them by
-- the Euclidean MST (Geometry.EMST — no hgeometry), sample a wire per edge.
-- The lifetime/date machinery, GridState dynamics, streamly and ConCat are all
-- dropped (they were unused by generateGrid). Deps: monad-bayes,
-- algebraic-graphs.
module Grid.Sample
  ( GridSpec'(..)
  , SampledGrid(..)
  , generateGrid
  ) where

import Control.Monad (replicateM)
import GHC.Generics (Generic)
import Data.Bifunctor (bimap)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Algebra.Graph.Labelled as LG

import Prob.Randomizable
import Physics.Units
import Physics.Transmission (TransmissionSpec, sampleTransmissionSpec)
import Grid.HH (HHSpec(..), NodeId, sampleHH)
import Geometry.EMST (minSpanTreeEdges, positiveGridPoints)

-- | Feeder spec: everything generateGrid forces.
data GridSpec' m = GridSpec
  { geometricOrigin :: m GeoC
  , nNodes          :: m Int
  , nodeDist        :: m Meters
  }

newtype SampledGrid = SampledGrid (LG.Graph TransmissionSpec HHSpec)
  deriving (Eq, Show, Generic)

mkSampledGrid :: LG.Graph TransmissionSpec HHSpec -> SampledGrid
mkSampledGrid = SampledGrid

data Node = Node
  { node      :: NodeId
  , coords    :: EuclideanC
  , geoCoords :: GeoC
  } deriving (Eq, Show, Generic)

instance Ord Node where
  compare (Node n1 _ _) (Node n2 _ _) = compare n1 n2

type AngularCoords = (Meters, BearingDeg)
type NodeDict = Map.Map NodeId Node
type Edge = (NodeId, NodeId)

generateGrid :: (MonadDistribution m) => GridSpec' m -> m SampledGrid
generateGrid GridSpec{..} = do
  ns <- nNodes
  go <- geometricOrigin
  angularCoords <- fmap (uncurry zip) $
    (,) <$> replicateM ns nodeDist <*> replicateM ns (uniform 0 360)
  let (nodeDict, tedges) = localAndGlobalLoc go angularCoords
      edgeLoc :: Edge -> (Node, Node)
      edgeLoc (x, y) = (nodeDict Map.! x, nodeDict Map.! y)
  gEdges <- mapM (uncurry sampleEdge) $ map edgeLoc tedges
  return $ mkSampledGrid $ LG.edges gEdges

dup :: a -> (a, a)
dup a = (a, a)

localAndGlobalLoc :: GeoC -> [AngularCoords] -> (NodeDict, [Edge])
localAndGlobalLoc center angularCoords = (nodeDict, edges)
  where
    radials = map (`radial` center) angularCoords
    ix = zip [(0 :: NodeId) ..] (positiveGridPoints angularCoords)
    xs = zipWith (\geoC (i, p) -> ((i, geoC), p)) radials ix
    edges = minSpanTreeEdges (NE.fromList ix)
    nodeDict = Map.fromList $ (bimap node id . dup . uncurry (uncurry mkNode)) <$> xs

mkNode :: Int -> GeoC -> EuclideanC -> Node
mkNode !i !g !c = Node i c g

sampleEdge :: (MonadDistribution m) => Node -> Node -> m (TransmissionSpec, HHSpec, HHSpec)
sampleEdge loc1 loc2 = (,,) <$> sampleTransmissionSpec <*> toHH loc1 <*> toHH loc2

toHH :: (MonadDistribution m) => Node -> m HHSpec
toHH Node{node, geoCoords, coords} = sampleHH node geoCoords coords

radial :: (Meters, BearingDeg) -> GeoC -> GeoC
radial (m, b) g = reverseHaversine g m b
