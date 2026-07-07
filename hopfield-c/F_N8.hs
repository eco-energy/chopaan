{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | AUTO-GENERATED graph-parameterized categorical Hopfield settle, size N=8.
-- The feeder (incidence B (N×E), conductances g, node P/Q injections) is DATA;
-- the kernel builds adjacency A=BᵀB, conductance-weighted coupling and external
-- input Θ=Bᵀ·inj, then runs K=8 Hopfield iterations (Manin-Marcolli Eq. 6.2),
-- unrolled into straight-line bindings so Categorifier lowers it to branch-free
-- C. mgenv feeders up to 8 nodes flow in as data (smaller ones zero-padded).
module F (hopfieldCategorified) where

import qualified Categorifier.C.CExpr.Cat as C
import Categorifier.C.CExpr.Cat.TargetOb (TargetOb)
import Categorifier.C.CTypes.CGeneric (CGeneric)
import qualified Categorifier.C.CTypes.CGeneric as CG
import Categorifier.C.CTypes.GArrays (GArrays)
import Categorifier.C.KTypes.C (C)
import qualified Categorifier.Categorify as Categorify
import Categorifier.Client (deriveHasRep)
import GHC.Generics (Generic)

data Input = Input
  { b0_0 :: C Double,
    b0_1 :: C Double,
    b0_2 :: C Double,
    b0_3 :: C Double,
    b0_4 :: C Double,
    b0_5 :: C Double,
    b0_6 :: C Double,
    b1_0 :: C Double,
    b1_1 :: C Double,
    b1_2 :: C Double,
    b1_3 :: C Double,
    b1_4 :: C Double,
    b1_5 :: C Double,
    b1_6 :: C Double,
    b2_0 :: C Double,
    b2_1 :: C Double,
    b2_2 :: C Double,
    b2_3 :: C Double,
    b2_4 :: C Double,
    b2_5 :: C Double,
    b2_6 :: C Double,
    b3_0 :: C Double,
    b3_1 :: C Double,
    b3_2 :: C Double,
    b3_3 :: C Double,
    b3_4 :: C Double,
    b3_5 :: C Double,
    b3_6 :: C Double,
    b4_0 :: C Double,
    b4_1 :: C Double,
    b4_2 :: C Double,
    b4_3 :: C Double,
    b4_4 :: C Double,
    b4_5 :: C Double,
    b4_6 :: C Double,
    b5_0 :: C Double,
    b5_1 :: C Double,
    b5_2 :: C Double,
    b5_3 :: C Double,
    b5_4 :: C Double,
    b5_5 :: C Double,
    b5_6 :: C Double,
    b6_0 :: C Double,
    b6_1 :: C Double,
    b6_2 :: C Double,
    b6_3 :: C Double,
    b6_4 :: C Double,
    b6_5 :: C Double,
    b6_6 :: C Double,
    b7_0 :: C Double,
    b7_1 :: C Double,
    b7_2 :: C Double,
    b7_3 :: C Double,
    b7_4 :: C Double,
    b7_5 :: C Double,
    b7_6 :: C Double,
    ip0 :: C Double,
    ip1 :: C Double,
    ip2 :: C Double,
    ip3 :: C Double,
    ip4 :: C Double,
    ip5 :: C Double,
    ip6 :: C Double,
    ip7 :: C Double,
    iq0 :: C Double,
    iq1 :: C Double,
    iq2 :: C Double,
    iq3 :: C Double,
    iq4 :: C Double,
    iq5 :: C Double,
    iq6 :: C Double,
    iq7 :: C Double,
    g0 :: C Double,
    g1 :: C Double,
    g2 :: C Double,
    g3 :: C Double,
    g4 :: C Double,
    g5 :: C Double,
    g6 :: C Double,
    x0p :: C Double,
    x0q :: C Double,
    x1p :: C Double,
    x1q :: C Double,
    x2p :: C Double,
    x2q :: C Double,
    x3p :: C Double,
    x3q :: C Double,
    x4p :: C Double,
    x4q :: C Double,
    x5p :: C Double,
    x5q :: C Double,
    x6p :: C Double,
    x6q :: C Double,
    alpha :: C Double
  }
  deriving (Generic)

deriveHasRep ''Input
instance CGeneric Input
instance GArrays C Input
type instance TargetOb Input = TargetOb (CG.Rep Input ())

data Output = Output
  { y0p :: C Double,
    y0q :: C Double,
    y1p :: C Double,
    y1q :: C Double,
    y2p :: C Double,
    y2q :: C Double,
    y3p :: C Double,
    y3q :: C Double,
    y4p :: C Double,
    y4q :: C Double,
    y5p :: C Double,
    y5q :: C Double,
    y6p :: C Double,
    y6q :: C Double,
    gridImport :: C Double
  }
  deriving (Generic)

deriveHasRep ''Output
instance CGeneric Output
instance GArrays C Output
type instance TargetOb Output = TargetOb (CG.Rep Output ())

coup :: C Double
coup = 0.1

pmax :: C Double
pmax = 100

clamp :: C Double -> C Double
clamp = max (negate pmax) . min pmax

f :: Input -> Output
f inp =
  let a = alpha inp
      upd old net = a * old + (1 - a) * clamp net
      thP0 = b0_0 inp * ip0 inp + b1_0 inp * ip1 inp + b2_0 inp * ip2 inp + b3_0 inp * ip3 inp + b4_0 inp * ip4 inp + b5_0 inp * ip5 inp + b6_0 inp * ip6 inp + b7_0 inp * ip7 inp
      thQ0 = b0_0 inp * iq0 inp + b1_0 inp * iq1 inp + b2_0 inp * iq2 inp + b3_0 inp * iq3 inp + b4_0 inp * iq4 inp + b5_0 inp * iq5 inp + b6_0 inp * iq6 inp + b7_0 inp * iq7 inp
      thP1 = b0_1 inp * ip0 inp + b1_1 inp * ip1 inp + b2_1 inp * ip2 inp + b3_1 inp * ip3 inp + b4_1 inp * ip4 inp + b5_1 inp * ip5 inp + b6_1 inp * ip6 inp + b7_1 inp * ip7 inp
      thQ1 = b0_1 inp * iq0 inp + b1_1 inp * iq1 inp + b2_1 inp * iq2 inp + b3_1 inp * iq3 inp + b4_1 inp * iq4 inp + b5_1 inp * iq5 inp + b6_1 inp * iq6 inp + b7_1 inp * iq7 inp
      thP2 = b0_2 inp * ip0 inp + b1_2 inp * ip1 inp + b2_2 inp * ip2 inp + b3_2 inp * ip3 inp + b4_2 inp * ip4 inp + b5_2 inp * ip5 inp + b6_2 inp * ip6 inp + b7_2 inp * ip7 inp
      thQ2 = b0_2 inp * iq0 inp + b1_2 inp * iq1 inp + b2_2 inp * iq2 inp + b3_2 inp * iq3 inp + b4_2 inp * iq4 inp + b5_2 inp * iq5 inp + b6_2 inp * iq6 inp + b7_2 inp * iq7 inp
      thP3 = b0_3 inp * ip0 inp + b1_3 inp * ip1 inp + b2_3 inp * ip2 inp + b3_3 inp * ip3 inp + b4_3 inp * ip4 inp + b5_3 inp * ip5 inp + b6_3 inp * ip6 inp + b7_3 inp * ip7 inp
      thQ3 = b0_3 inp * iq0 inp + b1_3 inp * iq1 inp + b2_3 inp * iq2 inp + b3_3 inp * iq3 inp + b4_3 inp * iq4 inp + b5_3 inp * iq5 inp + b6_3 inp * iq6 inp + b7_3 inp * iq7 inp
      thP4 = b0_4 inp * ip0 inp + b1_4 inp * ip1 inp + b2_4 inp * ip2 inp + b3_4 inp * ip3 inp + b4_4 inp * ip4 inp + b5_4 inp * ip5 inp + b6_4 inp * ip6 inp + b7_4 inp * ip7 inp
      thQ4 = b0_4 inp * iq0 inp + b1_4 inp * iq1 inp + b2_4 inp * iq2 inp + b3_4 inp * iq3 inp + b4_4 inp * iq4 inp + b5_4 inp * iq5 inp + b6_4 inp * iq6 inp + b7_4 inp * iq7 inp
      thP5 = b0_5 inp * ip0 inp + b1_5 inp * ip1 inp + b2_5 inp * ip2 inp + b3_5 inp * ip3 inp + b4_5 inp * ip4 inp + b5_5 inp * ip5 inp + b6_5 inp * ip6 inp + b7_5 inp * ip7 inp
      thQ5 = b0_5 inp * iq0 inp + b1_5 inp * iq1 inp + b2_5 inp * iq2 inp + b3_5 inp * iq3 inp + b4_5 inp * iq4 inp + b5_5 inp * iq5 inp + b6_5 inp * iq6 inp + b7_5 inp * iq7 inp
      thP6 = b0_6 inp * ip0 inp + b1_6 inp * ip1 inp + b2_6 inp * ip2 inp + b3_6 inp * ip3 inp + b4_6 inp * ip4 inp + b5_6 inp * ip5 inp + b6_6 inp * ip6 inp + b7_6 inp * ip7 inp
      thQ6 = b0_6 inp * iq0 inp + b1_6 inp * iq1 inp + b2_6 inp * iq2 inp + b3_6 inp * iq3 inp + b4_6 inp * iq4 inp + b5_6 inp * iq5 inp + b6_6 inp * iq6 inp + b7_6 inp * iq7 inp
      t0_1 = coup * (b0_0 inp * b0_1 inp + b1_0 inp * b1_1 inp + b2_0 inp * b2_1 inp + b3_0 inp * b3_1 inp + b4_0 inp * b4_1 inp + b5_0 inp * b5_1 inp + b6_0 inp * b6_1 inp + b7_0 inp * b7_1 inp) * g1 inp
      t0_2 = coup * (b0_0 inp * b0_2 inp + b1_0 inp * b1_2 inp + b2_0 inp * b2_2 inp + b3_0 inp * b3_2 inp + b4_0 inp * b4_2 inp + b5_0 inp * b5_2 inp + b6_0 inp * b6_2 inp + b7_0 inp * b7_2 inp) * g2 inp
      t0_3 = coup * (b0_0 inp * b0_3 inp + b1_0 inp * b1_3 inp + b2_0 inp * b2_3 inp + b3_0 inp * b3_3 inp + b4_0 inp * b4_3 inp + b5_0 inp * b5_3 inp + b6_0 inp * b6_3 inp + b7_0 inp * b7_3 inp) * g3 inp
      t0_4 = coup * (b0_0 inp * b0_4 inp + b1_0 inp * b1_4 inp + b2_0 inp * b2_4 inp + b3_0 inp * b3_4 inp + b4_0 inp * b4_4 inp + b5_0 inp * b5_4 inp + b6_0 inp * b6_4 inp + b7_0 inp * b7_4 inp) * g4 inp
      t0_5 = coup * (b0_0 inp * b0_5 inp + b1_0 inp * b1_5 inp + b2_0 inp * b2_5 inp + b3_0 inp * b3_5 inp + b4_0 inp * b4_5 inp + b5_0 inp * b5_5 inp + b6_0 inp * b6_5 inp + b7_0 inp * b7_5 inp) * g5 inp
      t0_6 = coup * (b0_0 inp * b0_6 inp + b1_0 inp * b1_6 inp + b2_0 inp * b2_6 inp + b3_0 inp * b3_6 inp + b4_0 inp * b4_6 inp + b5_0 inp * b5_6 inp + b6_0 inp * b6_6 inp + b7_0 inp * b7_6 inp) * g6 inp
      t1_0 = coup * (b0_1 inp * b0_0 inp + b1_1 inp * b1_0 inp + b2_1 inp * b2_0 inp + b3_1 inp * b3_0 inp + b4_1 inp * b4_0 inp + b5_1 inp * b5_0 inp + b6_1 inp * b6_0 inp + b7_1 inp * b7_0 inp) * g0 inp
      t1_2 = coup * (b0_1 inp * b0_2 inp + b1_1 inp * b1_2 inp + b2_1 inp * b2_2 inp + b3_1 inp * b3_2 inp + b4_1 inp * b4_2 inp + b5_1 inp * b5_2 inp + b6_1 inp * b6_2 inp + b7_1 inp * b7_2 inp) * g2 inp
      t1_3 = coup * (b0_1 inp * b0_3 inp + b1_1 inp * b1_3 inp + b2_1 inp * b2_3 inp + b3_1 inp * b3_3 inp + b4_1 inp * b4_3 inp + b5_1 inp * b5_3 inp + b6_1 inp * b6_3 inp + b7_1 inp * b7_3 inp) * g3 inp
      t1_4 = coup * (b0_1 inp * b0_4 inp + b1_1 inp * b1_4 inp + b2_1 inp * b2_4 inp + b3_1 inp * b3_4 inp + b4_1 inp * b4_4 inp + b5_1 inp * b5_4 inp + b6_1 inp * b6_4 inp + b7_1 inp * b7_4 inp) * g4 inp
      t1_5 = coup * (b0_1 inp * b0_5 inp + b1_1 inp * b1_5 inp + b2_1 inp * b2_5 inp + b3_1 inp * b3_5 inp + b4_1 inp * b4_5 inp + b5_1 inp * b5_5 inp + b6_1 inp * b6_5 inp + b7_1 inp * b7_5 inp) * g5 inp
      t1_6 = coup * (b0_1 inp * b0_6 inp + b1_1 inp * b1_6 inp + b2_1 inp * b2_6 inp + b3_1 inp * b3_6 inp + b4_1 inp * b4_6 inp + b5_1 inp * b5_6 inp + b6_1 inp * b6_6 inp + b7_1 inp * b7_6 inp) * g6 inp
      t2_0 = coup * (b0_2 inp * b0_0 inp + b1_2 inp * b1_0 inp + b2_2 inp * b2_0 inp + b3_2 inp * b3_0 inp + b4_2 inp * b4_0 inp + b5_2 inp * b5_0 inp + b6_2 inp * b6_0 inp + b7_2 inp * b7_0 inp) * g0 inp
      t2_1 = coup * (b0_2 inp * b0_1 inp + b1_2 inp * b1_1 inp + b2_2 inp * b2_1 inp + b3_2 inp * b3_1 inp + b4_2 inp * b4_1 inp + b5_2 inp * b5_1 inp + b6_2 inp * b6_1 inp + b7_2 inp * b7_1 inp) * g1 inp
      t2_3 = coup * (b0_2 inp * b0_3 inp + b1_2 inp * b1_3 inp + b2_2 inp * b2_3 inp + b3_2 inp * b3_3 inp + b4_2 inp * b4_3 inp + b5_2 inp * b5_3 inp + b6_2 inp * b6_3 inp + b7_2 inp * b7_3 inp) * g3 inp
      t2_4 = coup * (b0_2 inp * b0_4 inp + b1_2 inp * b1_4 inp + b2_2 inp * b2_4 inp + b3_2 inp * b3_4 inp + b4_2 inp * b4_4 inp + b5_2 inp * b5_4 inp + b6_2 inp * b6_4 inp + b7_2 inp * b7_4 inp) * g4 inp
      t2_5 = coup * (b0_2 inp * b0_5 inp + b1_2 inp * b1_5 inp + b2_2 inp * b2_5 inp + b3_2 inp * b3_5 inp + b4_2 inp * b4_5 inp + b5_2 inp * b5_5 inp + b6_2 inp * b6_5 inp + b7_2 inp * b7_5 inp) * g5 inp
      t2_6 = coup * (b0_2 inp * b0_6 inp + b1_2 inp * b1_6 inp + b2_2 inp * b2_6 inp + b3_2 inp * b3_6 inp + b4_2 inp * b4_6 inp + b5_2 inp * b5_6 inp + b6_2 inp * b6_6 inp + b7_2 inp * b7_6 inp) * g6 inp
      t3_0 = coup * (b0_3 inp * b0_0 inp + b1_3 inp * b1_0 inp + b2_3 inp * b2_0 inp + b3_3 inp * b3_0 inp + b4_3 inp * b4_0 inp + b5_3 inp * b5_0 inp + b6_3 inp * b6_0 inp + b7_3 inp * b7_0 inp) * g0 inp
      t3_1 = coup * (b0_3 inp * b0_1 inp + b1_3 inp * b1_1 inp + b2_3 inp * b2_1 inp + b3_3 inp * b3_1 inp + b4_3 inp * b4_1 inp + b5_3 inp * b5_1 inp + b6_3 inp * b6_1 inp + b7_3 inp * b7_1 inp) * g1 inp
      t3_2 = coup * (b0_3 inp * b0_2 inp + b1_3 inp * b1_2 inp + b2_3 inp * b2_2 inp + b3_3 inp * b3_2 inp + b4_3 inp * b4_2 inp + b5_3 inp * b5_2 inp + b6_3 inp * b6_2 inp + b7_3 inp * b7_2 inp) * g2 inp
      t3_4 = coup * (b0_3 inp * b0_4 inp + b1_3 inp * b1_4 inp + b2_3 inp * b2_4 inp + b3_3 inp * b3_4 inp + b4_3 inp * b4_4 inp + b5_3 inp * b5_4 inp + b6_3 inp * b6_4 inp + b7_3 inp * b7_4 inp) * g4 inp
      t3_5 = coup * (b0_3 inp * b0_5 inp + b1_3 inp * b1_5 inp + b2_3 inp * b2_5 inp + b3_3 inp * b3_5 inp + b4_3 inp * b4_5 inp + b5_3 inp * b5_5 inp + b6_3 inp * b6_5 inp + b7_3 inp * b7_5 inp) * g5 inp
      t3_6 = coup * (b0_3 inp * b0_6 inp + b1_3 inp * b1_6 inp + b2_3 inp * b2_6 inp + b3_3 inp * b3_6 inp + b4_3 inp * b4_6 inp + b5_3 inp * b5_6 inp + b6_3 inp * b6_6 inp + b7_3 inp * b7_6 inp) * g6 inp
      t4_0 = coup * (b0_4 inp * b0_0 inp + b1_4 inp * b1_0 inp + b2_4 inp * b2_0 inp + b3_4 inp * b3_0 inp + b4_4 inp * b4_0 inp + b5_4 inp * b5_0 inp + b6_4 inp * b6_0 inp + b7_4 inp * b7_0 inp) * g0 inp
      t4_1 = coup * (b0_4 inp * b0_1 inp + b1_4 inp * b1_1 inp + b2_4 inp * b2_1 inp + b3_4 inp * b3_1 inp + b4_4 inp * b4_1 inp + b5_4 inp * b5_1 inp + b6_4 inp * b6_1 inp + b7_4 inp * b7_1 inp) * g1 inp
      t4_2 = coup * (b0_4 inp * b0_2 inp + b1_4 inp * b1_2 inp + b2_4 inp * b2_2 inp + b3_4 inp * b3_2 inp + b4_4 inp * b4_2 inp + b5_4 inp * b5_2 inp + b6_4 inp * b6_2 inp + b7_4 inp * b7_2 inp) * g2 inp
      t4_3 = coup * (b0_4 inp * b0_3 inp + b1_4 inp * b1_3 inp + b2_4 inp * b2_3 inp + b3_4 inp * b3_3 inp + b4_4 inp * b4_3 inp + b5_4 inp * b5_3 inp + b6_4 inp * b6_3 inp + b7_4 inp * b7_3 inp) * g3 inp
      t4_5 = coup * (b0_4 inp * b0_5 inp + b1_4 inp * b1_5 inp + b2_4 inp * b2_5 inp + b3_4 inp * b3_5 inp + b4_4 inp * b4_5 inp + b5_4 inp * b5_5 inp + b6_4 inp * b6_5 inp + b7_4 inp * b7_5 inp) * g5 inp
      t4_6 = coup * (b0_4 inp * b0_6 inp + b1_4 inp * b1_6 inp + b2_4 inp * b2_6 inp + b3_4 inp * b3_6 inp + b4_4 inp * b4_6 inp + b5_4 inp * b5_6 inp + b6_4 inp * b6_6 inp + b7_4 inp * b7_6 inp) * g6 inp
      t5_0 = coup * (b0_5 inp * b0_0 inp + b1_5 inp * b1_0 inp + b2_5 inp * b2_0 inp + b3_5 inp * b3_0 inp + b4_5 inp * b4_0 inp + b5_5 inp * b5_0 inp + b6_5 inp * b6_0 inp + b7_5 inp * b7_0 inp) * g0 inp
      t5_1 = coup * (b0_5 inp * b0_1 inp + b1_5 inp * b1_1 inp + b2_5 inp * b2_1 inp + b3_5 inp * b3_1 inp + b4_5 inp * b4_1 inp + b5_5 inp * b5_1 inp + b6_5 inp * b6_1 inp + b7_5 inp * b7_1 inp) * g1 inp
      t5_2 = coup * (b0_5 inp * b0_2 inp + b1_5 inp * b1_2 inp + b2_5 inp * b2_2 inp + b3_5 inp * b3_2 inp + b4_5 inp * b4_2 inp + b5_5 inp * b5_2 inp + b6_5 inp * b6_2 inp + b7_5 inp * b7_2 inp) * g2 inp
      t5_3 = coup * (b0_5 inp * b0_3 inp + b1_5 inp * b1_3 inp + b2_5 inp * b2_3 inp + b3_5 inp * b3_3 inp + b4_5 inp * b4_3 inp + b5_5 inp * b5_3 inp + b6_5 inp * b6_3 inp + b7_5 inp * b7_3 inp) * g3 inp
      t5_4 = coup * (b0_5 inp * b0_4 inp + b1_5 inp * b1_4 inp + b2_5 inp * b2_4 inp + b3_5 inp * b3_4 inp + b4_5 inp * b4_4 inp + b5_5 inp * b5_4 inp + b6_5 inp * b6_4 inp + b7_5 inp * b7_4 inp) * g4 inp
      t5_6 = coup * (b0_5 inp * b0_6 inp + b1_5 inp * b1_6 inp + b2_5 inp * b2_6 inp + b3_5 inp * b3_6 inp + b4_5 inp * b4_6 inp + b5_5 inp * b5_6 inp + b6_5 inp * b6_6 inp + b7_5 inp * b7_6 inp) * g6 inp
      t6_0 = coup * (b0_6 inp * b0_0 inp + b1_6 inp * b1_0 inp + b2_6 inp * b2_0 inp + b3_6 inp * b3_0 inp + b4_6 inp * b4_0 inp + b5_6 inp * b5_0 inp + b6_6 inp * b6_0 inp + b7_6 inp * b7_0 inp) * g0 inp
      t6_1 = coup * (b0_6 inp * b0_1 inp + b1_6 inp * b1_1 inp + b2_6 inp * b2_1 inp + b3_6 inp * b3_1 inp + b4_6 inp * b4_1 inp + b5_6 inp * b5_1 inp + b6_6 inp * b6_1 inp + b7_6 inp * b7_1 inp) * g1 inp
      t6_2 = coup * (b0_6 inp * b0_2 inp + b1_6 inp * b1_2 inp + b2_6 inp * b2_2 inp + b3_6 inp * b3_2 inp + b4_6 inp * b4_2 inp + b5_6 inp * b5_2 inp + b6_6 inp * b6_2 inp + b7_6 inp * b7_2 inp) * g2 inp
      t6_3 = coup * (b0_6 inp * b0_3 inp + b1_6 inp * b1_3 inp + b2_6 inp * b2_3 inp + b3_6 inp * b3_3 inp + b4_6 inp * b4_3 inp + b5_6 inp * b5_3 inp + b6_6 inp * b6_3 inp + b7_6 inp * b7_3 inp) * g3 inp
      t6_4 = coup * (b0_6 inp * b0_4 inp + b1_6 inp * b1_4 inp + b2_6 inp * b2_4 inp + b3_6 inp * b3_4 inp + b4_6 inp * b4_4 inp + b5_6 inp * b5_4 inp + b6_6 inp * b6_4 inp + b7_6 inp * b7_4 inp) * g4 inp
      t6_5 = coup * (b0_6 inp * b0_5 inp + b1_6 inp * b1_5 inp + b2_6 inp * b2_5 inp + b3_6 inp * b3_5 inp + b4_6 inp * b4_5 inp + b5_6 inp * b5_5 inp + b6_6 inp * b6_5 inp + b7_6 inp * b7_5 inp) * g5 inp
      p0_0 = x0p inp
      q0_0 = x0q inp
      p1_0 = x1p inp
      q1_0 = x1q inp
      p2_0 = x2p inp
      q2_0 = x2q inp
      p3_0 = x3p inp
      q3_0 = x3q inp
      p4_0 = x4p inp
      q4_0 = x4q inp
      p5_0 = x5p inp
      q5_0 = x5q inp
      p6_0 = x6p inp
      q6_0 = x6q inp
      p0_1 = upd p0_0 (t0_1 * p1_0 + t0_2 * p2_0 + t0_3 * p3_0 + t0_4 * p4_0 + t0_5 * p5_0 + t0_6 * p6_0 + thP0)
      q0_1 = upd q0_0 (t0_1 * q1_0 + t0_2 * q2_0 + t0_3 * q3_0 + t0_4 * q4_0 + t0_5 * q5_0 + t0_6 * q6_0 + thQ0)
      p1_1 = upd p1_0 (t1_0 * p0_0 + t1_2 * p2_0 + t1_3 * p3_0 + t1_4 * p4_0 + t1_5 * p5_0 + t1_6 * p6_0 + thP1)
      q1_1 = upd q1_0 (t1_0 * q0_0 + t1_2 * q2_0 + t1_3 * q3_0 + t1_4 * q4_0 + t1_5 * q5_0 + t1_6 * q6_0 + thQ1)
      p2_1 = upd p2_0 (t2_0 * p0_0 + t2_1 * p1_0 + t2_3 * p3_0 + t2_4 * p4_0 + t2_5 * p5_0 + t2_6 * p6_0 + thP2)
      q2_1 = upd q2_0 (t2_0 * q0_0 + t2_1 * q1_0 + t2_3 * q3_0 + t2_4 * q4_0 + t2_5 * q5_0 + t2_6 * q6_0 + thQ2)
      p3_1 = upd p3_0 (t3_0 * p0_0 + t3_1 * p1_0 + t3_2 * p2_0 + t3_4 * p4_0 + t3_5 * p5_0 + t3_6 * p6_0 + thP3)
      q3_1 = upd q3_0 (t3_0 * q0_0 + t3_1 * q1_0 + t3_2 * q2_0 + t3_4 * q4_0 + t3_5 * q5_0 + t3_6 * q6_0 + thQ3)
      p4_1 = upd p4_0 (t4_0 * p0_0 + t4_1 * p1_0 + t4_2 * p2_0 + t4_3 * p3_0 + t4_5 * p5_0 + t4_6 * p6_0 + thP4)
      q4_1 = upd q4_0 (t4_0 * q0_0 + t4_1 * q1_0 + t4_2 * q2_0 + t4_3 * q3_0 + t4_5 * q5_0 + t4_6 * q6_0 + thQ4)
      p5_1 = upd p5_0 (t5_0 * p0_0 + t5_1 * p1_0 + t5_2 * p2_0 + t5_3 * p3_0 + t5_4 * p4_0 + t5_6 * p6_0 + thP5)
      q5_1 = upd q5_0 (t5_0 * q0_0 + t5_1 * q1_0 + t5_2 * q2_0 + t5_3 * q3_0 + t5_4 * q4_0 + t5_6 * q6_0 + thQ5)
      p6_1 = upd p6_0 (t6_0 * p0_0 + t6_1 * p1_0 + t6_2 * p2_0 + t6_3 * p3_0 + t6_4 * p4_0 + t6_5 * p5_0 + thP6)
      q6_1 = upd q6_0 (t6_0 * q0_0 + t6_1 * q1_0 + t6_2 * q2_0 + t6_3 * q3_0 + t6_4 * q4_0 + t6_5 * q5_0 + thQ6)
      p0_2 = upd p0_1 (t0_1 * p1_1 + t0_2 * p2_1 + t0_3 * p3_1 + t0_4 * p4_1 + t0_5 * p5_1 + t0_6 * p6_1 + thP0)
      q0_2 = upd q0_1 (t0_1 * q1_1 + t0_2 * q2_1 + t0_3 * q3_1 + t0_4 * q4_1 + t0_5 * q5_1 + t0_6 * q6_1 + thQ0)
      p1_2 = upd p1_1 (t1_0 * p0_1 + t1_2 * p2_1 + t1_3 * p3_1 + t1_4 * p4_1 + t1_5 * p5_1 + t1_6 * p6_1 + thP1)
      q1_2 = upd q1_1 (t1_0 * q0_1 + t1_2 * q2_1 + t1_3 * q3_1 + t1_4 * q4_1 + t1_5 * q5_1 + t1_6 * q6_1 + thQ1)
      p2_2 = upd p2_1 (t2_0 * p0_1 + t2_1 * p1_1 + t2_3 * p3_1 + t2_4 * p4_1 + t2_5 * p5_1 + t2_6 * p6_1 + thP2)
      q2_2 = upd q2_1 (t2_0 * q0_1 + t2_1 * q1_1 + t2_3 * q3_1 + t2_4 * q4_1 + t2_5 * q5_1 + t2_6 * q6_1 + thQ2)
      p3_2 = upd p3_1 (t3_0 * p0_1 + t3_1 * p1_1 + t3_2 * p2_1 + t3_4 * p4_1 + t3_5 * p5_1 + t3_6 * p6_1 + thP3)
      q3_2 = upd q3_1 (t3_0 * q0_1 + t3_1 * q1_1 + t3_2 * q2_1 + t3_4 * q4_1 + t3_5 * q5_1 + t3_6 * q6_1 + thQ3)
      p4_2 = upd p4_1 (t4_0 * p0_1 + t4_1 * p1_1 + t4_2 * p2_1 + t4_3 * p3_1 + t4_5 * p5_1 + t4_6 * p6_1 + thP4)
      q4_2 = upd q4_1 (t4_0 * q0_1 + t4_1 * q1_1 + t4_2 * q2_1 + t4_3 * q3_1 + t4_5 * q5_1 + t4_6 * q6_1 + thQ4)
      p5_2 = upd p5_1 (t5_0 * p0_1 + t5_1 * p1_1 + t5_2 * p2_1 + t5_3 * p3_1 + t5_4 * p4_1 + t5_6 * p6_1 + thP5)
      q5_2 = upd q5_1 (t5_0 * q0_1 + t5_1 * q1_1 + t5_2 * q2_1 + t5_3 * q3_1 + t5_4 * q4_1 + t5_6 * q6_1 + thQ5)
      p6_2 = upd p6_1 (t6_0 * p0_1 + t6_1 * p1_1 + t6_2 * p2_1 + t6_3 * p3_1 + t6_4 * p4_1 + t6_5 * p5_1 + thP6)
      q6_2 = upd q6_1 (t6_0 * q0_1 + t6_1 * q1_1 + t6_2 * q2_1 + t6_3 * q3_1 + t6_4 * q4_1 + t6_5 * q5_1 + thQ6)
      p0_3 = upd p0_2 (t0_1 * p1_2 + t0_2 * p2_2 + t0_3 * p3_2 + t0_4 * p4_2 + t0_5 * p5_2 + t0_6 * p6_2 + thP0)
      q0_3 = upd q0_2 (t0_1 * q1_2 + t0_2 * q2_2 + t0_3 * q3_2 + t0_4 * q4_2 + t0_5 * q5_2 + t0_6 * q6_2 + thQ0)
      p1_3 = upd p1_2 (t1_0 * p0_2 + t1_2 * p2_2 + t1_3 * p3_2 + t1_4 * p4_2 + t1_5 * p5_2 + t1_6 * p6_2 + thP1)
      q1_3 = upd q1_2 (t1_0 * q0_2 + t1_2 * q2_2 + t1_3 * q3_2 + t1_4 * q4_2 + t1_5 * q5_2 + t1_6 * q6_2 + thQ1)
      p2_3 = upd p2_2 (t2_0 * p0_2 + t2_1 * p1_2 + t2_3 * p3_2 + t2_4 * p4_2 + t2_5 * p5_2 + t2_6 * p6_2 + thP2)
      q2_3 = upd q2_2 (t2_0 * q0_2 + t2_1 * q1_2 + t2_3 * q3_2 + t2_4 * q4_2 + t2_5 * q5_2 + t2_6 * q6_2 + thQ2)
      p3_3 = upd p3_2 (t3_0 * p0_2 + t3_1 * p1_2 + t3_2 * p2_2 + t3_4 * p4_2 + t3_5 * p5_2 + t3_6 * p6_2 + thP3)
      q3_3 = upd q3_2 (t3_0 * q0_2 + t3_1 * q1_2 + t3_2 * q2_2 + t3_4 * q4_2 + t3_5 * q5_2 + t3_6 * q6_2 + thQ3)
      p4_3 = upd p4_2 (t4_0 * p0_2 + t4_1 * p1_2 + t4_2 * p2_2 + t4_3 * p3_2 + t4_5 * p5_2 + t4_6 * p6_2 + thP4)
      q4_3 = upd q4_2 (t4_0 * q0_2 + t4_1 * q1_2 + t4_2 * q2_2 + t4_3 * q3_2 + t4_5 * q5_2 + t4_6 * q6_2 + thQ4)
      p5_3 = upd p5_2 (t5_0 * p0_2 + t5_1 * p1_2 + t5_2 * p2_2 + t5_3 * p3_2 + t5_4 * p4_2 + t5_6 * p6_2 + thP5)
      q5_3 = upd q5_2 (t5_0 * q0_2 + t5_1 * q1_2 + t5_2 * q2_2 + t5_3 * q3_2 + t5_4 * q4_2 + t5_6 * q6_2 + thQ5)
      p6_3 = upd p6_2 (t6_0 * p0_2 + t6_1 * p1_2 + t6_2 * p2_2 + t6_3 * p3_2 + t6_4 * p4_2 + t6_5 * p5_2 + thP6)
      q6_3 = upd q6_2 (t6_0 * q0_2 + t6_1 * q1_2 + t6_2 * q2_2 + t6_3 * q3_2 + t6_4 * q4_2 + t6_5 * q5_2 + thQ6)
      p0_4 = upd p0_3 (t0_1 * p1_3 + t0_2 * p2_3 + t0_3 * p3_3 + t0_4 * p4_3 + t0_5 * p5_3 + t0_6 * p6_3 + thP0)
      q0_4 = upd q0_3 (t0_1 * q1_3 + t0_2 * q2_3 + t0_3 * q3_3 + t0_4 * q4_3 + t0_5 * q5_3 + t0_6 * q6_3 + thQ0)
      p1_4 = upd p1_3 (t1_0 * p0_3 + t1_2 * p2_3 + t1_3 * p3_3 + t1_4 * p4_3 + t1_5 * p5_3 + t1_6 * p6_3 + thP1)
      q1_4 = upd q1_3 (t1_0 * q0_3 + t1_2 * q2_3 + t1_3 * q3_3 + t1_4 * q4_3 + t1_5 * q5_3 + t1_6 * q6_3 + thQ1)
      p2_4 = upd p2_3 (t2_0 * p0_3 + t2_1 * p1_3 + t2_3 * p3_3 + t2_4 * p4_3 + t2_5 * p5_3 + t2_6 * p6_3 + thP2)
      q2_4 = upd q2_3 (t2_0 * q0_3 + t2_1 * q1_3 + t2_3 * q3_3 + t2_4 * q4_3 + t2_5 * q5_3 + t2_6 * q6_3 + thQ2)
      p3_4 = upd p3_3 (t3_0 * p0_3 + t3_1 * p1_3 + t3_2 * p2_3 + t3_4 * p4_3 + t3_5 * p5_3 + t3_6 * p6_3 + thP3)
      q3_4 = upd q3_3 (t3_0 * q0_3 + t3_1 * q1_3 + t3_2 * q2_3 + t3_4 * q4_3 + t3_5 * q5_3 + t3_6 * q6_3 + thQ3)
      p4_4 = upd p4_3 (t4_0 * p0_3 + t4_1 * p1_3 + t4_2 * p2_3 + t4_3 * p3_3 + t4_5 * p5_3 + t4_6 * p6_3 + thP4)
      q4_4 = upd q4_3 (t4_0 * q0_3 + t4_1 * q1_3 + t4_2 * q2_3 + t4_3 * q3_3 + t4_5 * q5_3 + t4_6 * q6_3 + thQ4)
      p5_4 = upd p5_3 (t5_0 * p0_3 + t5_1 * p1_3 + t5_2 * p2_3 + t5_3 * p3_3 + t5_4 * p4_3 + t5_6 * p6_3 + thP5)
      q5_4 = upd q5_3 (t5_0 * q0_3 + t5_1 * q1_3 + t5_2 * q2_3 + t5_3 * q3_3 + t5_4 * q4_3 + t5_6 * q6_3 + thQ5)
      p6_4 = upd p6_3 (t6_0 * p0_3 + t6_1 * p1_3 + t6_2 * p2_3 + t6_3 * p3_3 + t6_4 * p4_3 + t6_5 * p5_3 + thP6)
      q6_4 = upd q6_3 (t6_0 * q0_3 + t6_1 * q1_3 + t6_2 * q2_3 + t6_3 * q3_3 + t6_4 * q4_3 + t6_5 * q5_3 + thQ6)
      p0_5 = upd p0_4 (t0_1 * p1_4 + t0_2 * p2_4 + t0_3 * p3_4 + t0_4 * p4_4 + t0_5 * p5_4 + t0_6 * p6_4 + thP0)
      q0_5 = upd q0_4 (t0_1 * q1_4 + t0_2 * q2_4 + t0_3 * q3_4 + t0_4 * q4_4 + t0_5 * q5_4 + t0_6 * q6_4 + thQ0)
      p1_5 = upd p1_4 (t1_0 * p0_4 + t1_2 * p2_4 + t1_3 * p3_4 + t1_4 * p4_4 + t1_5 * p5_4 + t1_6 * p6_4 + thP1)
      q1_5 = upd q1_4 (t1_0 * q0_4 + t1_2 * q2_4 + t1_3 * q3_4 + t1_4 * q4_4 + t1_5 * q5_4 + t1_6 * q6_4 + thQ1)
      p2_5 = upd p2_4 (t2_0 * p0_4 + t2_1 * p1_4 + t2_3 * p3_4 + t2_4 * p4_4 + t2_5 * p5_4 + t2_6 * p6_4 + thP2)
      q2_5 = upd q2_4 (t2_0 * q0_4 + t2_1 * q1_4 + t2_3 * q3_4 + t2_4 * q4_4 + t2_5 * q5_4 + t2_6 * q6_4 + thQ2)
      p3_5 = upd p3_4 (t3_0 * p0_4 + t3_1 * p1_4 + t3_2 * p2_4 + t3_4 * p4_4 + t3_5 * p5_4 + t3_6 * p6_4 + thP3)
      q3_5 = upd q3_4 (t3_0 * q0_4 + t3_1 * q1_4 + t3_2 * q2_4 + t3_4 * q4_4 + t3_5 * q5_4 + t3_6 * q6_4 + thQ3)
      p4_5 = upd p4_4 (t4_0 * p0_4 + t4_1 * p1_4 + t4_2 * p2_4 + t4_3 * p3_4 + t4_5 * p5_4 + t4_6 * p6_4 + thP4)
      q4_5 = upd q4_4 (t4_0 * q0_4 + t4_1 * q1_4 + t4_2 * q2_4 + t4_3 * q3_4 + t4_5 * q5_4 + t4_6 * q6_4 + thQ4)
      p5_5 = upd p5_4 (t5_0 * p0_4 + t5_1 * p1_4 + t5_2 * p2_4 + t5_3 * p3_4 + t5_4 * p4_4 + t5_6 * p6_4 + thP5)
      q5_5 = upd q5_4 (t5_0 * q0_4 + t5_1 * q1_4 + t5_2 * q2_4 + t5_3 * q3_4 + t5_4 * q4_4 + t5_6 * q6_4 + thQ5)
      p6_5 = upd p6_4 (t6_0 * p0_4 + t6_1 * p1_4 + t6_2 * p2_4 + t6_3 * p3_4 + t6_4 * p4_4 + t6_5 * p5_4 + thP6)
      q6_5 = upd q6_4 (t6_0 * q0_4 + t6_1 * q1_4 + t6_2 * q2_4 + t6_3 * q3_4 + t6_4 * q4_4 + t6_5 * q5_4 + thQ6)
      p0_6 = upd p0_5 (t0_1 * p1_5 + t0_2 * p2_5 + t0_3 * p3_5 + t0_4 * p4_5 + t0_5 * p5_5 + t0_6 * p6_5 + thP0)
      q0_6 = upd q0_5 (t0_1 * q1_5 + t0_2 * q2_5 + t0_3 * q3_5 + t0_4 * q4_5 + t0_5 * q5_5 + t0_6 * q6_5 + thQ0)
      p1_6 = upd p1_5 (t1_0 * p0_5 + t1_2 * p2_5 + t1_3 * p3_5 + t1_4 * p4_5 + t1_5 * p5_5 + t1_6 * p6_5 + thP1)
      q1_6 = upd q1_5 (t1_0 * q0_5 + t1_2 * q2_5 + t1_3 * q3_5 + t1_4 * q4_5 + t1_5 * q5_5 + t1_6 * q6_5 + thQ1)
      p2_6 = upd p2_5 (t2_0 * p0_5 + t2_1 * p1_5 + t2_3 * p3_5 + t2_4 * p4_5 + t2_5 * p5_5 + t2_6 * p6_5 + thP2)
      q2_6 = upd q2_5 (t2_0 * q0_5 + t2_1 * q1_5 + t2_3 * q3_5 + t2_4 * q4_5 + t2_5 * q5_5 + t2_6 * q6_5 + thQ2)
      p3_6 = upd p3_5 (t3_0 * p0_5 + t3_1 * p1_5 + t3_2 * p2_5 + t3_4 * p4_5 + t3_5 * p5_5 + t3_6 * p6_5 + thP3)
      q3_6 = upd q3_5 (t3_0 * q0_5 + t3_1 * q1_5 + t3_2 * q2_5 + t3_4 * q4_5 + t3_5 * q5_5 + t3_6 * q6_5 + thQ3)
      p4_6 = upd p4_5 (t4_0 * p0_5 + t4_1 * p1_5 + t4_2 * p2_5 + t4_3 * p3_5 + t4_5 * p5_5 + t4_6 * p6_5 + thP4)
      q4_6 = upd q4_5 (t4_0 * q0_5 + t4_1 * q1_5 + t4_2 * q2_5 + t4_3 * q3_5 + t4_5 * q5_5 + t4_6 * q6_5 + thQ4)
      p5_6 = upd p5_5 (t5_0 * p0_5 + t5_1 * p1_5 + t5_2 * p2_5 + t5_3 * p3_5 + t5_4 * p4_5 + t5_6 * p6_5 + thP5)
      q5_6 = upd q5_5 (t5_0 * q0_5 + t5_1 * q1_5 + t5_2 * q2_5 + t5_3 * q3_5 + t5_4 * q4_5 + t5_6 * q6_5 + thQ5)
      p6_6 = upd p6_5 (t6_0 * p0_5 + t6_1 * p1_5 + t6_2 * p2_5 + t6_3 * p3_5 + t6_4 * p4_5 + t6_5 * p5_5 + thP6)
      q6_6 = upd q6_5 (t6_0 * q0_5 + t6_1 * q1_5 + t6_2 * q2_5 + t6_3 * q3_5 + t6_4 * q4_5 + t6_5 * q5_5 + thQ6)
      p0_7 = upd p0_6 (t0_1 * p1_6 + t0_2 * p2_6 + t0_3 * p3_6 + t0_4 * p4_6 + t0_5 * p5_6 + t0_6 * p6_6 + thP0)
      q0_7 = upd q0_6 (t0_1 * q1_6 + t0_2 * q2_6 + t0_3 * q3_6 + t0_4 * q4_6 + t0_5 * q5_6 + t0_6 * q6_6 + thQ0)
      p1_7 = upd p1_6 (t1_0 * p0_6 + t1_2 * p2_6 + t1_3 * p3_6 + t1_4 * p4_6 + t1_5 * p5_6 + t1_6 * p6_6 + thP1)
      q1_7 = upd q1_6 (t1_0 * q0_6 + t1_2 * q2_6 + t1_3 * q3_6 + t1_4 * q4_6 + t1_5 * q5_6 + t1_6 * q6_6 + thQ1)
      p2_7 = upd p2_6 (t2_0 * p0_6 + t2_1 * p1_6 + t2_3 * p3_6 + t2_4 * p4_6 + t2_5 * p5_6 + t2_6 * p6_6 + thP2)
      q2_7 = upd q2_6 (t2_0 * q0_6 + t2_1 * q1_6 + t2_3 * q3_6 + t2_4 * q4_6 + t2_5 * q5_6 + t2_6 * q6_6 + thQ2)
      p3_7 = upd p3_6 (t3_0 * p0_6 + t3_1 * p1_6 + t3_2 * p2_6 + t3_4 * p4_6 + t3_5 * p5_6 + t3_6 * p6_6 + thP3)
      q3_7 = upd q3_6 (t3_0 * q0_6 + t3_1 * q1_6 + t3_2 * q2_6 + t3_4 * q4_6 + t3_5 * q5_6 + t3_6 * q6_6 + thQ3)
      p4_7 = upd p4_6 (t4_0 * p0_6 + t4_1 * p1_6 + t4_2 * p2_6 + t4_3 * p3_6 + t4_5 * p5_6 + t4_6 * p6_6 + thP4)
      q4_7 = upd q4_6 (t4_0 * q0_6 + t4_1 * q1_6 + t4_2 * q2_6 + t4_3 * q3_6 + t4_5 * q5_6 + t4_6 * q6_6 + thQ4)
      p5_7 = upd p5_6 (t5_0 * p0_6 + t5_1 * p1_6 + t5_2 * p2_6 + t5_3 * p3_6 + t5_4 * p4_6 + t5_6 * p6_6 + thP5)
      q5_7 = upd q5_6 (t5_0 * q0_6 + t5_1 * q1_6 + t5_2 * q2_6 + t5_3 * q3_6 + t5_4 * q4_6 + t5_6 * q6_6 + thQ5)
      p6_7 = upd p6_6 (t6_0 * p0_6 + t6_1 * p1_6 + t6_2 * p2_6 + t6_3 * p3_6 + t6_4 * p4_6 + t6_5 * p5_6 + thP6)
      q6_7 = upd q6_6 (t6_0 * q0_6 + t6_1 * q1_6 + t6_2 * q2_6 + t6_3 * q3_6 + t6_4 * q4_6 + t6_5 * q5_6 + thQ6)
      p0_8 = upd p0_7 (t0_1 * p1_7 + t0_2 * p2_7 + t0_3 * p3_7 + t0_4 * p4_7 + t0_5 * p5_7 + t0_6 * p6_7 + thP0)
      q0_8 = upd q0_7 (t0_1 * q1_7 + t0_2 * q2_7 + t0_3 * q3_7 + t0_4 * q4_7 + t0_5 * q5_7 + t0_6 * q6_7 + thQ0)
      p1_8 = upd p1_7 (t1_0 * p0_7 + t1_2 * p2_7 + t1_3 * p3_7 + t1_4 * p4_7 + t1_5 * p5_7 + t1_6 * p6_7 + thP1)
      q1_8 = upd q1_7 (t1_0 * q0_7 + t1_2 * q2_7 + t1_3 * q3_7 + t1_4 * q4_7 + t1_5 * q5_7 + t1_6 * q6_7 + thQ1)
      p2_8 = upd p2_7 (t2_0 * p0_7 + t2_1 * p1_7 + t2_3 * p3_7 + t2_4 * p4_7 + t2_5 * p5_7 + t2_6 * p6_7 + thP2)
      q2_8 = upd q2_7 (t2_0 * q0_7 + t2_1 * q1_7 + t2_3 * q3_7 + t2_4 * q4_7 + t2_5 * q5_7 + t2_6 * q6_7 + thQ2)
      p3_8 = upd p3_7 (t3_0 * p0_7 + t3_1 * p1_7 + t3_2 * p2_7 + t3_4 * p4_7 + t3_5 * p5_7 + t3_6 * p6_7 + thP3)
      q3_8 = upd q3_7 (t3_0 * q0_7 + t3_1 * q1_7 + t3_2 * q2_7 + t3_4 * q4_7 + t3_5 * q5_7 + t3_6 * q6_7 + thQ3)
      p4_8 = upd p4_7 (t4_0 * p0_7 + t4_1 * p1_7 + t4_2 * p2_7 + t4_3 * p3_7 + t4_5 * p5_7 + t4_6 * p6_7 + thP4)
      q4_8 = upd q4_7 (t4_0 * q0_7 + t4_1 * q1_7 + t4_2 * q2_7 + t4_3 * q3_7 + t4_5 * q5_7 + t4_6 * q6_7 + thQ4)
      p5_8 = upd p5_7 (t5_0 * p0_7 + t5_1 * p1_7 + t5_2 * p2_7 + t5_3 * p3_7 + t5_4 * p4_7 + t5_6 * p6_7 + thP5)
      q5_8 = upd q5_7 (t5_0 * q0_7 + t5_1 * q1_7 + t5_2 * q2_7 + t5_3 * q3_7 + t5_4 * q4_7 + t5_6 * q6_7 + thQ5)
      p6_8 = upd p6_7 (t6_0 * p0_7 + t6_1 * p1_7 + t6_2 * p2_7 + t6_3 * p3_7 + t6_4 * p4_7 + t6_5 * p5_7 + thP6)
      q6_8 = upd q6_7 (t6_0 * q0_7 + t6_1 * q1_7 + t6_2 * q2_7 + t6_3 * q3_7 + t6_4 * q4_7 + t6_5 * q5_7 + thQ6)
      gridP = b0_0 inp * p0_8 + b0_1 inp * p1_8 + b0_2 inp * p2_8 + b0_3 inp * p3_8 + b0_4 inp * p4_8 + b0_5 inp * p5_8 + b0_6 inp * p6_8
  in Output { y0p = p0_8, y0q = q0_8, y1p = p1_8, y1q = q1_8, y2p = p2_8, y2q = q2_8, y3p = p3_8, y3q = q3_8, y4p = p4_8, y4q = q4_8, y5p = p5_8, y5q = q5_8, y6p = p6_8, y6q = q6_8, gridImport = gridP }

hopfieldCategorified :: Input `C.Cat` Output
hopfieldCategorified = Categorify.expression f
