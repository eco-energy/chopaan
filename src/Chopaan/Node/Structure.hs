{-# LANGUAGE TypeOperators, DataKinds, GADTs #-}
module Chopaan.Node.Structure where

import ConCat.Misc

data V
data I

data NodeF f where
  Grid :: (V :* I) -> (V :* I) :* (V:* I -> V:* I) -> NodeF f -- bidirectional converter
  Load :: V :* I -> NodeF f
  Generation :: V :* I -> V :* I -> NodeF f
  Storage :: (V :* I :+ V :* I) :+ (V :* I :+ V :* I) -> V :* I -> NodeF f
