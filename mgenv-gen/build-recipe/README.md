# mgenv-json build recipe (cabal, GHC 8.6.5) — progress + remaining blocker

Reproducible state for building mgenv's `Grid.Sample` graph generator with
cabal (no nix-build, no private deps). Drop these into an mgenv checkout.

## Applied (all non-destructive — no source modules deleted)

1. **Inlined opt-expect** — `RL_MDP.hs` → `src/RL/MDP.hs`,
   `Env_MonadEnv.hs` → `src/Env/MonadEnv.hs` (they only need `Streamly`),
   and removed `opt-expect` from `package.yaml` deps.
2. **Inlined lcirc** (private `git@bitbucket.org:ecoenergy/lcirc`, and the
   github mirror `eco-energy/lcirc` had drifted: `ConCat.LCirc`, no `Spider`).
   `LCirc/{LCirc,Cospan,Spider}.hs` → `src/LCirc/` as faithful self-contained
   modules (`VI` matching the original `(NodeId, Pair R)`; a real `Spider`
   Frobenius-fusion definition). `Grid/Grid.hs` uses nothing from them in live
   code, so they only need to compile. Removed `lCirc` from `package.yaml`.
3. **cabal.project** — removed the `concat-hardware` subdir (it pulls
   unresolvable `netlist-to-verilog`; mgenv only uses
   classes/examples/plugin/inline/known/satisfy).
4. **build-mgenv.sh** — provides the foreign libs `nix shell` doesn't inject:
   `libnuma` (GHC 8.6.5's RTS links `-lnuma`) and `zlib`
   (include/lib/pkgconfig), via `LIBRARY_PATH`/`C_INCLUDE_PATH`/
   `PKG_CONFIG_PATH` + cabal `--extra-{include,lib}-dirs`. Uses the
   haskell.nix `ghc-8.6.5` from the store + the committed `cabal.project.freeze`.

## Remaining blocker

`singletons-2.5.1` (pulled by `hgeometry`, which `Geometry.EMST` needs for the
Delaunay→MST graph generation) fails to compile on this GHC 8.6.5:

    src/Data/Singletons/Util.hs:99:16: error:
        Couldn't match type `Integer' with `Int'   (the NameU/Uniq pattern)

despite the freeze pinning the canonical 8.6.5 trio
(`singletons-2.5.1` / `th-desugar-1.9` / `th-abstraction-0.3.1.0`). This is
third-party bit-rot; `diagrams`/`servant`/`concat`'s `ConCat.Circuit` are
likely further walls in the full-library build.

## Note

The graph *schema* this would emit (`grids.json`: incidence `B`,
conductances `g`, node injections, per `GenJson.hs`) is already consumed by
`../../pufferlib-env/chopaan` and the categorified Hopfield kernel — so the
RL integration does not block on this build finishing.
