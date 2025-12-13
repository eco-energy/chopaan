# Chopaan SMT Solver Benchmark Results

**Date**: 2024-12-13
**Solver**: Z3 via SBV (Haskell)
**Algorithm**: Chopaan.Kibbutz.LinOpt.transportProblem (exact implementation)
**GHC**: 9.4.8

## Results

### Bipartite Transport Problem (full connectivity)

| Problem Size | Mean Time | Std Dev | R² |
|-------------|-----------|---------|-----|
| 4 nodes (2×2) | **40.02 ms** | 642.4 μs | 0.999 |
| 8 nodes (4×4) | **54.21 ms** | 1.815 ms | 0.996 |
| 16 nodes (8×8) | **114.9 ms** | 5.168 ms | 0.995 |
| 32 nodes (16×16) | **336.7 ms** | 25.09 ms | 0.987 |

### Sparse Mesh Transport (k=4 neighbors)

| Problem Size | Mean Time | Std Dev | R² |
|-------------|-----------|---------|-----|
| 12 nodes | **56.17 ms** | 2.179 ms | 0.996 |
| 24 nodes | **114.3 ms** | 7.618 ms | 0.998 |

## Algorithm Details

The benchmark uses the exact algorithm from `Chopaan.Kibbutz.LinOpt.transportProblem`:

```haskell
transportProblem :: Sources n -> Sinks n -> [[Double]] -> Symbolic ()
transportProblem ss ds cs = do
  vars <- txVars  -- Create flow variable matrix [[SReal]]
  -- Sink constraints: inflow >= demand
  mapM_ (\(xs, t) -> constrain $ sum xs .>= t) $ zip vars (getVals ds)
  -- Source constraints: outflow <= supply
  mapM_ (\(xs, t) -> constrain $ sum xs .<= t) $ zip (transpose vars) (getVals ss)
  -- Minimize cost via Hadamard product
  minimize "goal" $ sum $ fmap sum $ hadmard vars (fmap (fmap fromDouble) cs)
```

Key characteristics:
- Uses `SReal` (algebraic reals) for exact arithmetic
- Inequality constraints (>=, <=) allow Z3 optimization flexibility
- Lexicographic optimization via SBV
- Variable naming: `x_{source}_{sink}` for each flow edge

## Scaling Analysis

| Nodes | Time | Relative Slowdown |
|-------|------|------------------|
| 4→8 | 40ms→54ms | 1.35x |
| 8→16 | 54ms→115ms | 2.13x |
| 16→32 | 115ms→337ms | 2.93x |

The scaling is approximately O(n^1.5) to O(n^2), better than exponential due to:
- Inequality constraints providing optimization flexibility
- SBV's efficient translation to SMT-LIB
- Z3's linear arithmetic decision procedures

## Running the Benchmark

```bash
# Using nix-shell (handles SBV broken package issue)
export NIXPKGS_ALLOW_BROKEN=1
nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/nixos-23.11.tar.gz \
  -p 'haskellPackages.ghcWithPackages (ps: with ps; [criterion sbv containers deepseq])' z3 \
  --arg config '{ packageOverrides = pkgs: { haskellPackages = pkgs.haskellPackages.override { overrides = self: super: { sbv = pkgs.haskell.lib.dontCheck super.sbv; }; }; }; }'

# Compile
ghc -O2 -threaded -o solver-bench benchmark/SolverBench.hs

# Run
./solver-bench +RTS -N2 -RTS
```
