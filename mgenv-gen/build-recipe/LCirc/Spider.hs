{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Inlined Spider definition for lcirc (the mirror left `data Spider m n`
-- commented out). A spider is the Frobenius (co)multiplication generator that
-- fuses the m input wires and n output wires meeting at a network node — the
-- open-graph vertex used when composing LCirc cospans.
module LCirc.Spider
  ( Spider(..)
  , spider
  , fuse
  ) where

import GHC.Generics (Generic)

-- | An (m, n) spider carrying a node label @l@: m inputs, n outputs.
data Spider l = Spider
  { spiderIn    :: !Int
  , spiderOut   :: !Int
  , spiderLabel :: l
  } deriving (Eq, Show, Generic)

-- | The canonical (m, n) spider on a label.
spider :: Int -> Int -> l -> Spider l
spider = Spider

-- | Frobenius fusion: two spiders sharing a label merge, summing legs
-- (special commutative Frobenius algebra law).
fuse :: Spider l -> Spider l -> Spider l
fuse (Spider i1 o1 l) (Spider i2 o2 _) = Spider (i1 + i2) (o1 + o2) l
