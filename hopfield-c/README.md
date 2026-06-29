# hopfield-c — categorical Hopfield power flow, compiled to C

This compiles chopaan's categorical Hopfield dispatch dynamics directly to a
branch-free C function using [Categorifier](https://github.com/con-kitty/categorifier)
(`con-kitty/categorifier` + `con-kitty/categorifier-c`). No FFI marshalling: the
Haskell morphism *is* the C function.

## Why

`Chopaan.AC.HopfieldDynamics.hopfieldStep` realises Manin-Marcolli Eq. 6.2:

    X_e(n+1) = a * X_e(n) + (1 - a) * threshold( SUM_e' T_ee' X_e'(n) + Th_e )

These are morphisms in a category. Categorifier's whole purpose is lowering such
morphisms into a target category — here `Categorifier.C.CExpr.Cat.Cat`, whose
backend emits a C function. `F.hs` instantiates the step at fixed size (the
4-node radial test feeder, 3 edges, P/Q per edge) and unrolls K = 8 iterations
into straight-line arithmetic, so the generated kernel has no loops or branches.
That is the per-step physics for a PufferLib-style env at millions of steps/sec.

## Files

- `F.hs`    — `Input`/`Output` records over `Categorifier.C.KTypes.C`, the
             Hopfield `f :: Input -> Output`, and
             `hopfieldCategorified = Categorify.expression f :: Input `Cat` Output`.
- `Main.hs` — `writeCFiles "." "hopfield_step" hopfieldCategorified`, which emits
             `hopfield_step.c` / `hopfield_step.h`.

## Build (cabal)

The categorifier packages are not on Hackage; they are git
`source-repository-package`s in `categorifier-c`'s own `cabal.project`. The
least-friction route is to build inside that tree:

    # 1. fetch categorifier-c (GHC 8.10.1–9.0.x; this repo pins 9.0.1)
    git clone https://github.com/con-kitty/categorifier-c
    cd categorifier-c

    # 2. drop this example in and register it
    mkdir -p examples/hopfield
    cp /path/to/chopaan/hopfield-c/{F.hs,Main.hs} examples/hopfield/
    cat hopfield.cabal-stanza >> examples/categorifier-c-examples.cabal

    # 3. provide a compiler (e.g. via nix, default nixos cache)
    nix shell nixpkgs/nixos-21.11#haskell.compiler.ghc901 \
              nixpkgs/nixos-21.11#cabal-install --command bash

    # 4. build & run — cabal compiles categorifier/concat/sbv from source,
    #    then runs the generator
    cabal run hopfield

This writes `hopfield_step.c` and `hopfield_step.h` in the working directory.

To skip the from-source compile of the plugin closure, authorize the garnix
binary cache the flake is built with:

    extra-substituters = https://cache.garnix.io
    extra-trusted-public-keys = cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g=

then `nix develop` / `nix run` in the `categorifier-c` flake instead.

## Consuming the kernel

`hopfield_step.h` declares the generated entry point taking the input struct
(edge flows `x{0,1,2}{p,q}`, external inputs `t{0,1,2}{p,q}`, damping `alpha`)
and the output struct (settled flows `y{0,1,2}{p,q}`, `gridImport`). The
PufferLib env (`../chopaan_vec.h`) calls it once per `step()` in place of the
crude voltage proxy, so the env's physics is the categorified Hopfield attractor.
