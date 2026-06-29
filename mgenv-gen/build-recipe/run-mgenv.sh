#!/usr/bin/env bash
set -uo pipefail
export PATH="/nix/var/nix/profiles/default/bin:$PATH"
GHC865=/nix/store/rxz9xi0frclm2jcibrynlpsdsdfj9z39-ghc-8.6.5
NUMALIB=/nix/store/dgb0w5fsdym9k2hazvnbhsknrbmbi8a2-numactl-2.0.13/lib
ZOUT=/nix/store/2yy85x1bhwmynzmpr4n29caxpfm0bkk4-zlib-1.2.12
export CABAL_DIR=/tmp/cabal-mgenv HOME=/tmp/cabal-mgenv
cd /tmp/mgenv
nix shell nixpkgs/nixos-21.11#cabal-install nixpkgs/nixos-21.11#gmp \
  nixpkgs/nixos-21.11#zlib --command bash -c "
    export PATH=$GHC865/bin:\$PATH
    export LD_LIBRARY_PATH=$NUMALIB:$ZOUT/lib:$GHC865/lib/ghc-8.6.5/rts:\${LD_LIBRARY_PATH:-}
    cabal run -v0 exe:mgenv-json -w $GHC865/bin/ghc \
      --extra-lib-dirs=$ZOUT/lib --extra-lib-dirs=$NUMALIB -- 4 /tmp/mgenv/grids.json 2>&1
    echo RUN_EXIT=\$?
  "
