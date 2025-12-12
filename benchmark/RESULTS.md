# SMT Solver Benchmark Results

**Date**: 2024-12-12
**Solver**: Z3 via SBV (Haskell)
**GHC**: 9.4.8
**Problem**: Transport flow optimization (bipartite graph, minimize cost)

## Results

### Bipartite Transport Problem

| Problem Size | Mean Time | Std Dev | Notes |
|-------------|-----------|---------|-------|
| 4 nodes (2 sources × 2 sinks) | **1.267s** | 46.83ms | R² = 0.998 |
| 8 nodes (4 sources × 4 sinks) | **15.86s** | 96.64ms | R² = 1.000 |
| 16 nodes (8 sources × 8 sinks) | >7min | - | In progress |

### Key Observations

1. **Exponential Scaling**: The solver exhibits exponential time complexity:
   - 4 nodes → 8 nodes: **12.5x slowdown** (1.27s → 15.86s)
   - Each doubling of problem size results in >10x increase in solve time

2. **SMT Optimization Characteristics**:
   - Full bipartite connectivity creates O(n²) flow variables
   - Each variable requires non-negativity constraints
   - Conservation constraints for sources and sinks
   - Cost minimization objective

3. **Solver Statistics**:
   - Variance is low (R² ≈ 1.000), indicating consistent performance
   - Outliers introduce 19% variance inflation

## Running the Benchmark

```bash
# Enter nix-shell with dependencies
export NIXPKGS_ALLOW_BROKEN=1
nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/nixos-23.11.tar.gz \
  -p 'haskellPackages.ghcWithPackages (ps: with ps; [criterion sbv containers deepseq])' z3 \
  --arg config '{ packageOverrides = pkgs: { haskellPackages = pkgs.haskellPackages.override { overrides = self: super: { sbv = pkgs.haskell.lib.dontCheck super.sbv; }; }; }; }'

# Compile
ghc -O2 -threaded -o solver-bench benchmark/SolverBench.hs

# Run (warning: larger tests take exponentially longer)
./solver-bench --output benchmark-results.html +RTS -N2 -RTS
```

## Notes

- The benchmark uses Criterion for statistical analysis
- sbv package requires `NIXPKGS_ALLOW_BROKEN=1` and `dontCheck` override to skip tests
- Larger problem sizes (32, 64, 128, 256 nodes) would take hours or days to complete
