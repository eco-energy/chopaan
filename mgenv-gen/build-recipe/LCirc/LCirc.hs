{-# LANGUAGE DeriveGeneric #-}

-- | Inlined from eco-energy/lcirc (module LCirc.LCirc).
-- Minimal surface mgenv's Grid.Grid imports (`import LCirc.LCirc hiding (VI)`).
-- VI is the vertex-injection type; original: newtype VI = VI (NodeId, Pair R).
module LCirc.LCirc
  ( VI(..)
  ) where

import GHC.Generics (Generic)

-- | Vertex injection: a node id paired with a (real, imag) value, as in lcirc.
newtype VI = VI (Int, (Double, Double))
  deriving (Eq, Ord, Show, Generic)
