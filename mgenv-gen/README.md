# mgenv-gen — distribution-grid graphs via mgenv, built with cabal

Generates the grid topologies the chopaan dispatch env runs on, using
[mgenv](https://github.com/eco-energy/mgenv)'s own sampling library
(`Grid.Sample`), built and run **with cabal** (not nix).

## What it produces

`GenJson.hs` calls mgenv's real API:

    spec <- sampleThis                 -- Randomizable (GridSpec' SamplerIO)
    grid <- generateGrid spec          -- SampledGrid = Graph TransmissionSpec HHSpec

and flattens each sampled grid to dense node indices with per-edge wire
resistance and per-node PV / load, emitting `{ "grids": [...] }`. Topology is a
Euclidean minimum spanning tree of radially-placed households — a radial
distribution feeder. (`GenJson.hs` replaces mgenv's bit-rotted `app/Main.hs`,
which calls the two-argument `sampleGridSpec` with no arguments and does not
typecheck.)

## GHC scope

mgenv is GHC **8.6.5** (stack `lts-14.17`: ConCat, streamly, hgeometry,
monad-bayes all pinned to that era). This is the **same compiler as the
chopaan library** and a *different* one from `../hopfield-c` (categorifier,
GHC 9.0.1) — the two cannot share one cabal build, so they live as sibling
cabal projects in this repo.

## Build (cabal)

mgenv is not on Hackage; build inside its tree with this generator dropped in:

    git clone https://github.com/eco-energy/mgenv && cd mgenv
    mkdir -p app && cp /path/to/chopaan/mgenv-gen/GenJson.hs app/GenJson.hs
    cat /path/to/chopaan/mgenv-gen/mgenv-json.cabal-stanza >> package.yaml  # hpack
    # provide GHC 8.6.5 + cabal (e.g. via nix, default nixos cache)
    cabal run mgenv-json -- 64 grids.json

This writes `grids.json` (64 sampled feeders) for the env to load. cabal
compiles mgenv's closure (hgeometry/Delaunay, streamly, monad-bayes, ConCat)
from source.

## How the env consumes it

`grids.json` carries, per grid: `num_nodes`, `edge_src`/`edge_tgt`,
`edge_r_ohm` / `edge_r_pu`, `node_type` (`slack`/`prosumer`),
`node_p_rated_kw`, `node_p_load_kw`, `node_q_load_kvar`. The PufferLib env
allocates per-env Hopfield edge state over each grid's topology and settles it
with the categorified kernel from `../hopfield-c` — so mgenv supplies the
*topology* and categorifier supplies the *physics*, both within chopaan.
