#!/usr/bin/env bash
# Build the chopaan ocean env two ways. Run from this directory.
set -e
PYINC=$(python3 -c "import sysconfig;print(sysconfig.get_path('include'))")
NPINC=$(python3 -c "import numpy;print(numpy.get_include())")

# 1) native throughput demo (no Python): categorified Hopfield physics
gcc -O3 -march=native -I. chopaan_demo.c -lm -o chopaan_demo
echo "native demo:"; ./chopaan_demo

# 2) PufferLib CPython extension (what setup.py's Extension(binding.c) builds)
gcc -O3 -shared -fPIC -I. -I"$PYINC" -I"$NPINC" \
    -DNPY_NO_DEPRECATED_API=NPY_1_7_API_VERSION binding.c -lm -o binding.so
echo "built binding.so"
