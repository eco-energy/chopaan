{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

-- | Euclidean minimum spanning tree — reimplemented WITHOUT hgeometry.
--
-- The original used hgeometry's Delaunay triangulation then MST. hgeometry
-- stops at 0.9.0.0 on Hackage (GHC 8.6 / singletons-2.5) and cannot move to
-- 9.0.1. But the EMST of a point set is exactly the MST of its complete
-- Euclidean graph, so Prim's O(n^2) over the points gives the identical tree
-- with no external geometry dependency. Same public API as the callers use.
module Geometry.EMST (minSpanTreeEdges, pathToEdges, positiveGridPoints) where

import qualified Data.List.NonEmpty as NE
import           Data.List (minimumBy)
import           Data.Ord (comparing)
import           Physics.Units (Meters, BearingDeg, EuclideanC, toRadians)

-- | EMST edges over labelled 2-D points (Prim's on the complete graph).
minSpanTreeEdges :: (Ord a) => NE.NonEmpty (a, EuclideanC) -> [(a, a)]
minSpanTreeEdges pts0 =
  case NE.toList pts0 of
    []          -> []
    (p0 : rest) -> go [p0] rest []
  where
    d2 (_, (x1, y1)) (_, (x2, y2)) = (x1 - x2) ** 2 + (y1 - y2) ** 2
    go _ [] acc = reverse acc
    go inTree outside acc =
      let (u, v) = minimumBy (comparing (\(a, b) -> d2 a b))
                     [ (a, b) | a <- inTree, b <- outside ]
      in go (v : inTree) (filter ((/= fst v) . fst) outside)
            ((fst u, fst v) : acc)

pathToEdges :: [a] -> [(a, a)]
pathToEdges p = zip p (tail p)

-- | Polar (radius, bearing) -> cartesian, shifted so all coords are >= 0.
positiveGridPoints :: [(Meters, BearingDeg)] -> [EuclideanC]
positiveGridPoints = shift . map toCartesian

shift :: [(Meters, Meters)] -> [(Meters, Meters)]
shift ps = zip (map ((+ rightwards ps) . fst) ps)
               (map ((+ upwards ps)    . snd) ps)

rightwards, upwards :: [EuclideanC] -> Meters
rightwards = abs . foldl min 0 . map fst
upwards    = abs . foldl min 0 . map snd

toCartesian :: (Meters, BearingDeg) -> (Meters, Meters)
toCartesian (r, th) = (r * sin (toRadians th), r * cos (toRadians th))
