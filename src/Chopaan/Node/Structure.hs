{-# LANGUAGE TypeOperators, DataKinds, GADTs #-}
module Chopaan.Node.Structure where

import ConCat.Misc

data V
data I


data Node= Node
  { grid :: (V :* I) -> (V :* I) :* (V:* I-> V:* I) -- bidirectional converter
  , load :: V :* I-- draw 
  , generation :: V :* I-> V :* I-- charger
  , storage :: (V :* I :+ V :* I) :+ (V :* I :+ V :* I) -> V :* I
  }
