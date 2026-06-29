# mgenv-json build recipe (cabal, GHC 8.6.5) — COMPLETE

`mgenv-json` builds and runs with cabal, generating real mgenv distribution
feeders (`../sample-output/grids.json`). No nix-build, no private deps.
Everything here reproduces it; `mgenv-src.patch` is the full diff vs the
pristine mgenv tarball.

## Result

`cabal run mgenv-json -- 4 grids.json` produced 4 radial feeders (9–15 nodes),
each a connected Euclidean-MST tree with real copper-wire resistances, PV
ratings (50–500 W), and household loads — mgenv's actual `Grid.Sample`
(`sampleGridSpec`→`generateGrid`, Delaunay→MST via hgeometry).

## 14 blockers cleared (all non-destructive — no source modules deleted)

1. **opt-expect inlined** — `RL_MDP.hs`→`src/RL/MDP.hs`,
   `Env_MonadEnv.hs`→`src/Env/MonadEnv.hs`; dep dropped from package.yaml.
2. **lcirc inlined** (private bitbucket; github mirror had drifted to
   `ConCat.LCirc` with no `Spider`) — `LCirc/{LCirc,Cospan,Spider}.hs`→
   `src/LCirc/` (faithful `VI`, real `Spider` Frobenius def). dep dropped.
3. **concat-hardware** subdir removed from cabal.project (unresolvable
   `netlist-to-verilog`).
4. **libnuma** on the link path (GHC 8.6.5 RTS links `-lnuma`).
5. **zlib** include/lib/pkgconfig (`nix shell` doesn't inject C-lib paths).
6. **singletons-2.5.1** patched (`singletons-2.5.1-Util.patch`): this GHC's
   `template-haskell` has `Uniq = Integer`, so `qNewUnique`'s `return n`
   needed `fromIntegral`. Vendored as a local package.
7. **transformers-base / monad-control / exceptions** added (the inlined
   `MonadEnv` instances need them); dropped its unused, streamly-git-only
   `sampleStream`.
8. **streamly 0.6.1 → 0.8.0** in the freeze — `stackage-to-hackage` missed
   stack.yaml's git override; mgenv's code uses 0.8.0 internals.
9. **Grid.HH** `hhS`/`mkHH` stubbed — household *dynamics* (unused by
   generation) whose streamly Pipe applicative targets a streamly-git API.
10. **Grid.HH** `runHH` defined — exported but never defined upstream.
11. **Grid.Sample** `absToUTC` import fixed — it lives in `Physics.Time`,
    not `Physics.Units` (mgenv import bug).
12. **Grid.Sample** dynamics scaffolding (`GridState` free `t`,
    `evolveGridState`, `f`, `actor`, `initGridState`) reduced to compiling
    stubs — all unused by generation.
13. **Grid.Sample** `tedges`→`edges` typo fixed — the MST-edge return value.
14. **Grid.Viz / World.Server** excluded from `mgenv.cabal`'s module list
    (files kept on disk) — diagrams/servant rendering, orthogonal to
    generation and bit-rotted; plus a `TransmissionState{..}` record-wildcard
    fix in Viz.

## GenJson.hs

Builds the `GridSpec'` directly rather than via `Randomizable`'s `sampleThis`
(mgenv's `Randomizable GeoC`/`LifeTime` instances are unimplemented and crash
at runtime). `generateGrid` only forces `nNodes`/`geometricOrigin`/`nodeDist`,
so `startDate`/`rate` are left `undefined`. `nNodes` capped to `[6..16]` (set
`[4..4]` for env-sized feeders).

## Reproduce

    # in a fresh mgenv checkout, apply mgenv-src.patch, drop in these
    # cabal.project / cabal.project.freeze / mgenv.cabal / package.yaml /
    # app/GenJson.hs / vendor/singletons-2.5.1 (patched), then:
    ./build-mgenv.sh     # builds exe:mgenv-json (GHC 8.6.5 + freeze + foreign libs)
    ./run-mgenv.sh       # cabal run mgenv-json -- N grids.json
