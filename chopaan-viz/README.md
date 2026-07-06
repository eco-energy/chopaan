# chopaan-viz — a Dear ImGui grid playground

Play with chopaan's power-flow feeders like a game. The **renderee is an
[algebraic-graphs](https://hackage.haskell.org/package/algebraic-graphs)
`Graph Int`** (a real mgenv 4-node feeder); every frame it is packed and settled
by the **categorified Hopfield kernel over FFI** — `cbits/hopfield_step.c` is the
exact C that Categorifier lowered chopaan's Hopfield step (`hopfield-c/F.hs`) to,
the same morphism the PufferLib RL env runs. So the on-screen physics *is* the
lowered categorical dynamics, not a reimplementation.

    ┌ Feeder.hs ── grids_4node.json (real mgenv feeders)
    │
    ├ Main.hs ──── Algebra.Graph.Graph Int  (drag / rewire / tune)
    │                     │  pack incidence B + injections + g + α
    │                     ▼
    └ Kernel.hs ── foreign import "hopfield_step"  →  cbits/hopfield_step.c
                          │  settled edge flows (P,Q) + grid import
                          ▼
                   Dear ImGui draw list  (nodes, flow-coloured edges, score)

## What you can do

- **Drag nodes** with the mouse (amber = PV bus, blue = slack/grid-tie, grey = load).
- **Rewire** — the topology is an algebraic graph; `Next feeder` cycles the real
  mgenv-sampled feeders, each a distinct Euclidean-MST tree.
- **Tune** the PV setpoint, damping `α`, per-edge conductance, and time-of-day.
  Edges recolour live by settled flow (green = out, red = in; thicker = more kW).
- **Score** — the `cost` readout is grid-import × tariff; play to minimise it
  (curtail PV midday, ride the evening peak, etc.).

## Build

Needs SDL2 + an OpenGL loader on the system, plus the `dear-imgui` package
(which vendors the Dear ImGui C++ sources).

    # system libs (Debian/Ubuntu): sudo apt install libsdl2-dev libgl1-mesa-dev
    # macOS: brew install sdl2
    # or a nix shell:  nix-shell -p SDL2 SDL2.dev libGL pkg-config

    cd chopaan-viz
    cabal run chopaan-viz          # loads ./grids_4node.json

`grids_4node.json` (real mgenv output) ships alongside; without it the app falls
back to a built-in demo feeder.

## Version note (dear-imgui API)

This targets `dear-imgui` ~2.2. Three spots are the only version-sensitive bits;
adjust if your build complains:

- **`ImU32`** — assumed a `Word32` (`col` in `Main.hs`). If yours is a newtype,
  use its constructor instead of `fromIntegral`.
- **`DearImGui.Raw.DrawList.addText`** — used for node labels; if absent in your
  version, delete that one line in `drawGraph`.
- **Draw-list float args** are `CFloat`, sizes `CInt` — already typed as such.

## Why FFI, not a Haskell port

The categorical Hopfield step lives in one place — `hopfield-c/F.hs` — and is
lowered to C once by Categorifier. Both the RL env and this renderer call that C,
so they can never drift. The graph you edit here is the *input* to that morphism;
Dear ImGui is just the lens.
