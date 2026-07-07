# mgenv migration: GHC 8.6.5 -> 9.0.1

Goal: build mgenv's *generation* core on GHC 9.0.1 so the dear-imgui renderer
(9.0.1) can `import Grid.Sample` directly, removing the GHC-version wall (today
Feeder shells out to the 8.6.5 `mgenv-json` binary).

## Feasibility (analysed)

No unportable walls remain:

- **hgeometry** — the only hard blocker (maxes at 0.9.0.0 / singletons-2.5 on
  Hackage, no 9.0.1 version). Used ONLY for Delaunay->MST, which is the
  Euclidean MST. REIMPLEMENTED with Prim's in `src/Geometry/EMST.hs` — same API,
  zero geometry deps. ✔
- **ConCat** — appears in 4 modules but only in dynamics/aspirational code
  (the unused/unexported `HH` GADT; `ConCat.Isomorphism` in the transmission
  pipes; `RAD`/`Category` in `Physics.Time`). None in the generation path →
  strip those functions + imports; replace `ConCat.Misc` `:*`/`:+` with
  `(,)`/`Either`.
- **streamly / astro / dimensional** — only in dynamics (`runPV` sun position,
  battery ODE, household Pipes). `generateGrid`/`sampleHH`/`sampleXSpec` don't
  need them. Strip each module to spec-type + sampler.
- **sampleGridSpec / startDate / GridState / lifeTime** — UNUSED (GenJson builds
  `GridSpec'` directly). Drop them, which also drops `RL.MDP`/`Env.MonadEnv`/
  `Physics.Time`.
- **monad-bayes** 0.1.1.0 -> 1.3.x — real but bounded API port
  (`MonadSample` -> `MonadDistribution`; `Sampler` -> `Sampler.Strict`).

## Resulting closure (deps: monad-bayes + algebraic-graphs + base + containers)

Grid.Sample (generation core only) · Grid.HH (HHSpec + sampleHH) ·
Geometry.EMST (Prim's) · Physics.Units (pure Double reimpl) ·
Physics.{Transmission,PV,Consumption,Storage} (spec + sampler only) ·
Prob.Randomizable (monad-bayes-ported).

## Status
- [x] EMST reimplemented (hgeometry dropped)
- [x] Physics.Units pure reimpl (drop astro)
- [x] strip dynamics/ConCat/streamly from samplers + Grid.HH + Grid.Sample
- [x] monad-bayes 1.1 port (MonadSample -> MonadDistribution)
- [x] builds clean on GHC 9.0.1 (all 9 modules, BUILD_EXIT=0)
- [x] wired into chopaan-viz: Feeder imports Grid.Sample; renderer links + BUILD_EXIT=0
