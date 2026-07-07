# Confirmed build (compiles + links; verified headless)

chopaan-viz builds as a real ELF against dear-imgui 2.1.3 + the migrated mgenv.
The renderer is GHC 9.0.1. System libs (from nixpkgs pin nixos-21.11): SDL2
2.0.14, libGL/libglvnd, GLEW 2.2, GLU 9.0.2, and the X11 stack (libX11, Xext,
Xcursor, Xi, Xrandr, Xfixes, XScrnSaver, Xxf86vm, xcb, Xrender + xorgproto
headers).

Solver constraints that resolve for GHC 9.0.1 + SDL2 2.0.14:

    cabal build \
      --index-state=2023-06-01T00:00:00Z \
      --constraint="dear-imgui <2.2" \   # 2.1.3; 2.2+ needs GHC 9.2+
      --constraint="sdl2 <2.5.4" \        # 2.5.3.3; 2.5.5 needs SDL_Vertex (SDL2>=2.0.18)
      chopaan-viz

`cabal.project` pulls the migrated mgenv (`../mgenv-gen/migrate-9.0.1`) as a
local package, so `Feeder` links `Grid.Sample` and samples feeders in-process.
Resulting closure: dear-imgui-2.1.3, sdl2-2.5.3.3, monad-bayes-1.1.0, mgenv-gen9.

Cannot *run* a GUI in a headless container, but the full binary links — the
dear-imgui API usage and the direct mgenv import are compiler-verified.
