{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Categorical Hopfield power-flow settling, compiled to C.
--
-- This is a fixed-size instantiation of chopaan's
-- @Chopaan.AC.HopfieldDynamics.hopfieldStep@ (Manin-Marcolli Eq. 6.2):
--
--   X_e(n+1) = a * X_e(n) + (1 - a) * threshold( SUM_e' T_ee' X_e'(n) + Th_e )
--
-- specialised to the 4-node radial test feeder with 3 undirected edges:
--
--     0 ──e0── 1 ──e1── 2
--              │
--              e2
--              │
--              3
--
-- All three edges meet at node 1, so each edge couples to the other two
-- (T_ee' = tCoup for e' /= e, sharing node 1). We track real and reactive
-- power (P, Q) on each edge; the threshold is the categorical feasibility
-- gate (.)+ realised as a box clamp to [-pmax, pmax].
--
-- @settle@ unrolls K = 8 Hopfield iterations into straight-line arithmetic,
-- which Categorifier lowers to a branch-free C function — the fast per-step
-- kernel for the dispatch env.

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

-- | Edge flows X plus per-edge external input Theta and the damping a.
data Input = Input
  { -- current edge flows (P, Q) for e0, e1, e2
    x0p :: C Double, x0q :: C Double,
    x1p :: C Double, x1q :: C Double,
    x2p :: C Double, x2q :: C Double,
    -- external input Theta per edge (from node injections / action / loads)
    t0p :: C Double, t0q :: C Double,
    t1p :: C Double, t1q :: C Double,
    t2p :: C Double, t2q :: C Double,
    -- damping factor a in (0, 1)
    alpha :: C Double
  }
  deriving (Generic)

deriveHasRep ''Input

instance CGeneric Input

instance GArrays C Input

type instance TargetOb Input = TargetOb (CG.Rep Input ())

-- | Settled edge flows and a reward proxy (slack-edge real power = grid import).
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

-- Coupling weight between adjacent edges (all share node 1).
tCoup :: C Double
tCoup = 0.3

-- Per-edge flow magnitude cap; the categorical (.)+ feasibility gate.
pmax :: C Double
pmax = 100

clamp :: C Double -> C Double
clamp = max (negate pmax) . min pmax

type Flows = (C Double, C Double, C Double, C Double, C Double, C Double)

f :: Input -> Output
f inp =
  let a = alpha inp
      -- one Hopfield iteration over the three edges
      oneStep :: Flows -> Flows
      oneStep (p0, q0, p1, q1, p2, q2) =
        let upd old net = a * old + (1 - a) * clamp net
            -- e0 couples to e1, e2; external input Theta_e0
            n0p = upd p0 (tCoup * (p1 + p2) + t0p inp)
            n0q = upd q0 (tCoup * (q1 + q2) + t0q inp)
            n1p = upd p1 (tCoup * (p0 + p2) + t1p inp)
            n1q = upd q1 (tCoup * (q0 + q2) + t1q inp)
            n2p = upd p2 (tCoup * (p0 + p1) + t2p inp)
            n2q = upd q2 (tCoup * (q0 + q1) + t2q inp)
         in (n0p, n0q, n1p, n1q, n2p, n2q)

      -- unroll K = 8 iterations toward the Hopfield attractor
      settle = oneStep . oneStep . oneStep . oneStep
             . oneStep . oneStep . oneStep . oneStep

      (f0p, f0q, f1p, f1q, f2p, f2q) =
        settle (x0p inp, x0q inp, x1p inp, x1q inp, x2p inp, x2q inp)
   in Output
        { y0p = f0p, y0q = f0q,
          y1p = f1p, y1q = f1q,
          y2p = f2p, y2q = f2q,
          -- slack edge e0 real-power flow ~ net grid import at the tie point
          gridImport = f0p
        }

hopfieldCategorified :: Input `C.Cat` Output
hopfieldCategorified = Categorify.expression f
