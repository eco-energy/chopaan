#!/usr/bin/env bash
# Data-driven kernel build: the categorified kernel's size is NOT hardcoded — it
# is the maximum feeder size mgenv actually samples. One number, computed once
# from the mgenv output, flows to the F.hs codegen, the categorified C, and the
# generated grid table (MGENV_N in mgenv_grids.h). Change mgenv's nNodes
# distribution and the kernel dimension follows automatically.
#
#   mgenv sample ─► N = max(num_nodes) ─► gen_fhs.py N ─► Categorifier ─► C
#                        └─────────────► gen_grids_h.py N (padded) ─► env/viz
#
# Usage: build_kernel.sh  (expects the mgenv-json exe + categorifier-c toolchain
# already built; see mgenv-gen/build-recipe and hopfield-c/README).
set -euo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
GRIDS="${GRIDS:-/tmp/mgenv/grids.json}"
CATDIR="${CATDIR:-/tmp/categorifier-c}"

echo "[1/5] sample feeders with mgenv"
: "${SKIP_MGENV:=0}"
[ "$SKIP_MGENV" = 1 ] || /tmp/run-mgenv.sh >/dev/null 2>&1 || true   # -> $GRIDS

# [2/5] kernel size = the max feeder mgenv produced (compile-time constant)
N=$(python3 -c "import json,sys; d=json.load(open('$GRIDS'))['grids']; print(max(g['num_nodes'] for g in d))")
echo "[2/5] N (kernel size, from the mgenv sample) = $N"
echo "$N" > "$HERE/KERNEL_N"

echo "[3/5] codegen F.hs at N=$N and categorify"
python3 "$HERE/gen_fhs.py" "$N" 8 > "$CATDIR/examples/hopfield/F.hs"
( cd "$CATDIR" && /tmp/build-hopfield.sh ) >/tmp/cat-build.log 2>&1 || { echo "categorify failed; see /tmp/cat-build.log"; exit 1; }
cp "$CATDIR/hopfield_step.c" "$CATDIR/hopfield_step.h" "$HERE/generated/"

echo "[4/5] codegen padded grid table (MGENV_N=$N)"
python3 "$HERE/gen_grids_h.py" "$GRIDS" "$N" > "$HERE/generated/mgenv_grids.h"

echo "[5/5] propagate to env + renderer"
for dst in /home/user/chopaan/pufferlib-env/chopaan /home/user/chopaan/chopaan-viz/cbits; do
  cp "$HERE/generated/hopfield_step.c" "$HERE/generated/hopfield_step.h" "$dst/" 2>/dev/null || true
done
cp "$HERE/generated/mgenv_grids.h" /home/user/chopaan/pufferlib-env/chopaan/ 2>/dev/null || true
echo "done. kernel + grids are size N=$N (single source: KERNEL_N)."
