#!/usr/bin/env python3
# Codegen a fixed-size graph-parameterized Hopfield F.hs for Categorifier.
# N nodes, E = N-1 edge slots, K settling iterations. No tuples (they cap out) —
# the unroll is straight-line named bindings, so it scales to real feeder sizes.
import sys
N = int(sys.argv[1]) if len(sys.argv) > 1 else 8
E = N - 1
K = int(sys.argv[2]) if len(sys.argv) > 2 else 8

def bfield(n,e): return f"b{n}_{e}"
def ipf(n): return f"ip{n}"
def iqf(n): return f"iq{n}"
def gf(e): return f"g{e}"
def xpf(e): return f"x{e}p"
def xqf(e): return f"x{e}q"

# record fields in a fixed order == C input_double order
fields = []
for n in range(N):
    for e in range(E):
        fields.append(bfield(n,e))
for n in range(N): fields.append(ipf(n))
for n in range(N): fields.append(iqf(n))
for e in range(E): fields.append(gf(e))
for e in range(E):
    fields.append(xpf(e)); fields.append(xqf(e))
fields.append("alpha")

out_fields = []
for e in range(E):
    out_fields.append(f"y{e}p"); out_fields.append(f"y{e}q")
out_fields.append("gridImport")

def field_block(fs):
    return ",\n    ".join(f"{f} :: C Double" for f in fs)

L = []
ap = L.append
ap("""{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | AUTO-GENERATED graph-parameterized categorical Hopfield settle, size N=%d.
-- The feeder (incidence B (N×E), conductances g, node P/Q injections) is DATA;
-- the kernel builds adjacency A=BᵀB, conductance-weighted coupling and external
-- input Θ=Bᵀ·inj, then runs K=%d Hopfield iterations (Manin-Marcolli Eq. 6.2),
-- unrolled into straight-line bindings so Categorifier lowers it to branch-free
-- C. mgenv feeders up to %d nodes flow in as data (smaller ones zero-padded).
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
""" % (N, K, N))

ap("data Input = Input\n  { " + field_block(fields) + "\n  }\n  deriving (Generic)\n")
ap("deriveHasRep ''Input\ninstance CGeneric Input\ninstance GArrays C Input")
ap("type instance TargetOb Input = TargetOb (CG.Rep Input ())\n")

ap("data Output = Output\n  { " + field_block(out_fields) + "\n  }\n  deriving (Generic)\n")
ap("deriveHasRep ''Output\ninstance CGeneric Output\ninstance GArrays C Output")
ap("type instance TargetOb Output = TargetOb (CG.Rep Output ())\n")

ap("""coup :: C Double
coup = 0.1

pmax :: C Double
pmax = 100

clamp :: C Double -> C Double
clamp = max (negate pmax) . min pmax
""")

# f body
b = []
b.append("f :: Input -> Output")
b.append("f inp =")
b.append("  let a = alpha inp")
b.append("      upd old net = a * old + (1 - a) * clamp net")
# theta
for e in range(E):
    tP = " + ".join(f"{bfield(n,e)} inp * {ipf(n)} inp" for n in range(N))
    tQ = " + ".join(f"{bfield(n,e)} inp * {iqf(n)} inp" for n in range(N))
    b.append(f"      thP{e} = {tP}")
    b.append(f"      thQ{e} = {tQ}")
# coupling t_e_e'  (adjacency A_ee' = sum_n b_ne b_ne', times coup*g_e')
for e in range(E):
    for e2 in range(E):
        if e == e2: continue
        adj = " + ".join(f"{bfield(n,e)} inp * {bfield(n,e2)} inp" for n in range(N))
        b.append(f"      t{e}_{e2} = coup * ({adj}) * {gf(e2)} inp")
# initial flows
for e in range(E):
    b.append(f"      p{e}_0 = {xpf(e)} inp")
    b.append(f"      q{e}_0 = {xqf(e)} inp")
# K iterations
for k in range(1, K+1):
    for e in range(E):
        netP = " + ".join([f"t{e}_{e2} * p{e2}_{k-1}" for e2 in range(E) if e2!=e] + [f"thP{e}"])
        netQ = " + ".join([f"t{e}_{e2} * q{e2}_{k-1}" for e2 in range(E) if e2!=e] + [f"thQ{e}"])
        b.append(f"      p{e}_{k} = upd p{e}_{k-1} ({netP})")
        b.append(f"      q{e}_{k} = upd q{e}_{k-1} ({netQ})")
# grid import (net P leaving slack node 0)
gimp = " + ".join(f"{bfield(0,e)} inp * p{e}_{K}" for e in range(E))
b.append(f"      gridP = {gimp}")
# output record
outs = []
for e in range(E):
    outs.append(f"y{e}p = p{e}_{K}")
    outs.append(f"y{e}q = q{e}_{K}")
outs.append("gridImport = gridP")
b.append("  in Output { " + ", ".join(outs) + " }")
ap("\n".join(b))
ap("\nhopfieldCategorified :: Input `C.Cat` Output\nhopfieldCategorified = Categorify.expression f")

sys.stdout.write("\n".join(L) + "\n")
