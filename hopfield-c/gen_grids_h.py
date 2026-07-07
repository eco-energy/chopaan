#!/usr/bin/env python3
# Codegen mgenv_grids.h from an mgenv grids.json, padded to a fixed kernel size
# N (= MGENV_N). Variable-size feeders (<= N nodes) are zero-padded, so the one
# fixed-size categorified kernel settles all of them. N is passed in (derived
# from the sample by build_kernel.sh) — nothing here is hardcoded.
import sys, json

grids_path = sys.argv[1]
N = int(sys.argv[2])
E = N - 1
d = json.load(open(grids_path))["grids"]
# keep only feeders that fit the kernel
d = [g for g in d if g["num_nodes"] <= N]

L = []
L += ["// AUTO-GENERATED from mgenv grids.json, padded to the sampled max size.",
      "// Node 0 = slack, node 1 = PV (action-controlled), others = load.",
      "#ifndef MGENV_GRIDS_H", "#define MGENV_GRIDS_H",
      f"#define MGENV_N {N}", f"#define MGENV_E {E}", f"#define MGENV_NUM_GRIDS {len(d)}",
      "typedef struct {",
      "  double B[MGENV_N][MGENV_E];   // directed incidence (padded)",
      "  double g[MGENV_E];            // per-edge conductance, normalized",
      "  double p_rated;               // PV rating at node 1 (kW)",
      "  double p_load_base[MGENV_N];  // per-node base load (kW)",
      "  int    n_active;              // real node count of this feeder",
      "} MgenvGrid;",
      f"static const MgenvGrid MGENV_GRIDS[{len(d)}] = {{"]

for g in d:
    na = g["num_nodes"]
    src, tgt = g["edge_src"], g["edge_tgt"]
    rohm = g["edge_r_ohm"]
    cond = [1.0 / max(r, 1e-9) for r in rohm]
    m = max(cond) if cond else 1.0
    cond = [c / m for c in cond]
    # incidence B[N][E], padded
    B = [[0.0] * E for _ in range(N)]
    for e, (s, t) in enumerate(zip(src, tgt)):
        if e < E and s < N and t < N:
            B[s][e] = 1.0; B[t][e] = -1.0
    gpad = (cond + [0.0] * E)[:E]
    rated = g["node_p_rated_kw"][1] if na > 1 else 0.0
    load = (g["node_p_load_kw"] + [0.0] * N)[:N]
    load[0] = 0.0   # slack draws no load
    Bs = "{" + ",".join("{" + ",".join(f"{B[n][e]:.0f}" for e in range(E)) + "}" for n in range(N)) + "}"
    gs = "{" + ",".join(f"{c:.4f}" for c in gpad) + "}"
    ls = "{" + ",".join(f"{v:.4f}" for v in load) + "}"
    L.append(f"  {{ {Bs}, {gs}, {rated:.4f}, {ls}, {na} }},")

L += ["};", "#endif // MGENV_GRIDS_H"]
sys.stdout.write("\n".join(L) + "\n")
