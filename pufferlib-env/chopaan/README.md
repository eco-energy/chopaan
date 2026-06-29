# chopaan — PufferLib ocean env with categorified Hopfield physics

A grid-dispatch RL env whose **per-step physics is the categorical Hopfield
power-flow kernel** lowered to C by Categorifier (`hopfield_step.c`), running
over an **mgenv-generated feeder topology** supplied as data.

## The full pipeline

    mgenv (Haskell, GHC 8.6.5)        chopaan/HopfieldDynamics (Haskell, GHC 9.0.1)
    Grid.Sample -> incidence B,             F.hs : Input -> Output
    conductances g, injections              Categorify.expression  (plugin)
            │                                        │
            │  (graph as data)                       ▼  writeCFiles
            └──────────────►  input_double[30] ──►  hopfield_step.c   (branch-free C)
                                                     │
                                  chopaan.h c_step packs B,g,injections, calls
                                  hopfield_step(), settles edge flows, prices import
                                                     │
                                  binding.c  ◄── env_binding.h ──►  PufferLib vec API
                                                     │
                                  chopaan.py : pufferlib.PufferEnv (Box obs/action)

The graph mgenv produces flows through the categorified C as the 4×3 incidence
matrix `B`, edge conductances `g = 1/R`, and node injections — so the *topology*
is mgenv's and the *dynamics* are the lowered categorical Hopfield morphism
(Manin-Marcolli Eq. 6.2, 8 settling iterations unrolled). See `../../hopfield-c`
for how `hopfield_step.c` is generated.

## Build (`make_puffernet` / setup.py path)

`./build.sh` builds both targets:

1. **Native throughput demo** (`chopaan_demo.c`, no Python) — steps the env with
   random actions and reports steps/sec. Measured **~2.8 M steps/sec**
   single-threaded with the full Hopfield settle as physics.
2. **PufferLib CPython extension** (`binding.c`) — exactly what
   `setup.py`'s `Extension('pufferlib.ocean.chopaan.binding', ['binding.c'])`
   compiles. Produces `binding.so` exposing `vec_init/vec_reset/vec_step/
   vec_log/vec_render/vec_close`.

To use inside a PufferLib checkout, drop this directory at
`pufferlib/ocean/chopaan/` and `pip install -e .` (or run the project build);
`chopaan.py`'s `Chopaan(pufferlib.PufferEnv)` then works with the standard
vector trainer. Verified end-to-end (4 vec envs, 24-step episodes, returns ≈ −72).

## Files

- `chopaan.h`     — env struct + `c_reset`/`c_step` (packs the kernel input,
                    settles, prices grid import), `c_render`/`c_close` stubs.
- `hopfield_step.{c,h}` — the categorified Hopfield kernel (generated).
- `binding.c`     — `env_binding.h` glue; pulls the kernel into the extension TU.
- `chopaan.py`    — `pufferlib.PufferEnv` wrapper, Box(6) obs / Box(2) action.
- `chopaan_demo.c`— native rollout + steps/sec benchmark.
- `build.sh`      — builds both targets.

## Env semantics

- **Obs (6):** hour/24, solar/rated, the two prosumer loads (normalized),
  previous grid import, tariff.
- **Action (2):** PV real/reactive setpoint at node 1, normalized `[-1, 1]`
  (curtailed by available solar).
- **Reward:** `-(import_cost/100 + 10·voltage_violation)` with the K-Electric
  peak/off-peak tariff; grid import comes from the settled slack-edge flow.
- **Episode:** 24 hourly steps, auto-reset on terminal.

## Wiring mgenv grids (next)

`ch_default_grid` hardcodes the 4-node test feeder. A `my_put` in `binding.c`
can stream an mgenv grid (`grids.json` → incidence `B` + conductances `g` +
base loads) per env so each parallel env trains on a different sampled feeder.
