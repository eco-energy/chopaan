{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Graph-parameterized categorical Hopfield power-flow settling, compiled to C.
--
-- The graph mgenv generates flows in as DATA: a 4x3 directed incidence matrix
-- B (B[n][e] = +1 if edge e leaves node n, -1 if it enters, 0 otherwise), the
-- per-edge conductance g_e = 1/R_e, and the per-node P/Q injections. The kernel
-- builds the coupling operator from the graph and runs chopaan's
-- @HopfieldDynamics.hopfieldStep@ (Manin-Marcolli Eq. 6.2):
--
--   X_e(n+1) = a * X_e(n) + (1 - a) * threshold( SUM_e' T_ee' X_e'(n) + Th_e )
--
-- where, derived from the incidence matrix,
--   adjacency   A_ee'  = SUM_n B[n][e] B[n][e']        (edges sharing a node)
--   coupling    T_ee'  = COUP * A_ee' * g_e'           (e' /= e)
--   ext. input  Th_e   = SUM_n B[n][e] inj[n]          (= inj[src] - inj[tgt])
--   grid import        = SUM_e B[0][e] X_e^P           (net P leaving slack node 0)
--
-- So the SAME categorified C function works over any 4-node / 3-edge topology
-- mgenv produces (larger grids reduce onto a fixed (N,E) frame). Everything is
-- fixed-size dense arithmetic, which Categorifier lowers to a branch-free C
-- function. K = 8 settling iterations are unrolled.
--
-- input_double layout (record field order, 30 doubles):
--   b00 b01 b02  b10 b11 b12  b20 b21 b22  b30 b31 b32     -- incidence B (row-major, node-major)
--   ip0 ip1 ip2 ip3   iq0 iq1 iq2 iq3                      -- node P and Q injections
--   g0 g1 g2                                               -- edge conductances
--   x0p x0q x1p x1q x2p x2q                                -- initial edge flows
--   alpha                                                  -- damping
-- output_double layout (7 doubles): y0p y0q y1p y1q y2p y2q  gridImport

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
  { -- incidence matrix B, node-major: b<n><e>
    b00 :: C Double, b01 :: C Double, b02 :: C Double,
    b10 :: C Double, b11 :: C Double, b12 :: C Double,
    b20 :: C Double, b21 :: C Double, b22 :: C Double,
    b30 :: C Double, b31 :: C Double, b32 :: C Double,
    -- node injections (real / reactive)
    ip0 :: C Double, ip1 :: C Double, ip2 :: C Double, ip3 :: C Double,
    iq0 :: C Double, iq1 :: C Double, iq2 :: C Double, iq3 :: C Double,
    -- edge conductances g_e = 1/R_e
    g0 :: C Double, g1 :: C Double, g2 :: C Double,
    -- initial edge flows (P, Q)
    x0p :: C Double, x0q :: C Double,
    x1p :: C Double, x1q :: C Double,
    x2p :: C Double, x2q :: C Double,
    -- damping
    alpha :: C Double
  }
  deriving (Generic)

deriveHasRep ''Input

instance CGeneric Input

instance GArrays C Input

type instance TargetOb Input = TargetOb (CG.Rep Input ())

data Output = Output
  { y0p :: C Double, y0q :: C Double,
    y1p :: C Double, y1q :: C Double,
    y2p :: C Double, y2q :: C Double,
    gridImport :: C Double
  }
  deriving (Generic)

deriveHasRep ''Output

instance CGeneric Output

instance GArrays C Output

type instance TargetOb Output = TargetOb (CG.Rep Output ())

-- Global coupling scale; the categorical (.)+ feasibility cap.
coup :: C Double
coup = 0.1

pmax :: C Double
pmax = 100

clamp :: C Double -> C Double
clamp = max (negate pmax) . min pmax

type Flows = (C Double, C Double, C Double, C Double, C Double, C Double)

f :: Input -> Output
f inp =
  let a = alpha inp
      -- incidence columns (one per edge)
      (c00, c10, c20, c30) = (b00 inp, b10 inp, b20 inp, b30 inp)  -- edge 0
      (c01, c11, c21, c31) = (b01 inp, b11 inp, b21 inp, b31 inp)  -- edge 1
      (c02, c12, c22, c32) = (b02 inp, b12 inp, b22 inp, b32 inp)  -- edge 2

      -- adjacency A_ee' = dot of incidence columns (shared nodes)
      a01 = c00*c01 + c10*c11 + c20*c21 + c30*c31
      a02 = c00*c02 + c10*c12 + c20*c22 + c30*c32
      a12 = c01*c02 + c11*c12 + c21*c22 + c31*c32

      -- conductance-weighted coupling T_ee' = coup * A_ee' * g_e'
      (gg0, gg1, gg2) = (g0 inp, g1 inp, g2 inp)
      t01 = coup * a01 * gg1;  t02 = coup * a02 * gg2   -- into edge 0 from 1,2
      t10 = coup * a01 * gg0;  t12 = coup * a12 * gg2   -- into edge 1 from 0,2
      t20 = coup * a02 * gg0;  t21 = coup * a12 * gg1   -- into edge 2 from 0,1

      -- external input Th_e = SUM_n B[n][e] * inj[n]
      (jp0, jp1, jp2, jp3) = (ip0 inp, ip1 inp, ip2 inp, ip3 inp)
      (jq0, jq1, jq2, jq3) = (iq0 inp, iq1 inp, iq2 inp, iq3 inp)
      th0p = c00*jp0 + c10*jp1 + c20*jp2 + c30*jp3
      th1p = c01*jp0 + c11*jp1 + c21*jp2 + c31*jp3
      th2p = c02*jp0 + c12*jp1 + c22*jp2 + c32*jp3
      th0q = c00*jq0 + c10*jq1 + c20*jq2 + c30*jq3
      th1q = c01*jq0 + c11*jq1 + c21*jq2 + c31*jq3
      th2q = c02*jq0 + c12*jq1 + c22*jq2 + c32*jq3

      upd old net = a * old + (1 - a) * clamp net

      oneStep :: Flows -> Flows
      oneStep (p0, q0, p1, q1, p2, q2) =
        ( upd p0 (t01*p1 + t02*p2 + th0p)
        , upd q0 (t01*q1 + t02*q2 + th0q)
        , upd p1 (t10*p0 + t12*p2 + th1p)
        , upd q1 (t10*q0 + t12*q2 + th1q)
        , upd p2 (t20*p0 + t21*p1 + th2p)
        , upd q2 (t20*q0 + t21*q1 + th2q)
        )

      settle = oneStep . oneStep . oneStep . oneStep
             . oneStep . oneStep . oneStep . oneStep

      (f0p, f0q, f1p, f1q, f2p, f2q) =
        settle (x0p inp, x0q inp, x1p inp, x1q inp, x2p inp, x2q inp)

      gridP = c00*f0p + c01*f1p + c02*f2p   -- net P leaving slack node 0
   in Output
        { y0p = f0p, y0q = f0q,
          y1p = f1p, y1q = f1q,
          y2p = f2p, y2q = f2q,
          gridImport = gridP
        }

hopfieldCategorified :: Input `C.Cat` Output
hopfieldCategorified = Categorify.expression f
