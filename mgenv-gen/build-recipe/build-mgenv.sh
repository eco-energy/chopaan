#!/usr/bin/env bash
set -uo pipefail
export PATH="/nix/var/nix/profiles/default/bin:$PATH"
GHC865=/nix/store/rxz9xi0frclm2jcibrynlpsdsdfj9z39-ghc-8.6.5
NUMALIB=/nix/store/dgb0w5fsdym9k2hazvnbhsknrbmbi8a2-numactl-2.0.13/lib
ZDEV=/nix/store/2y10rqa796j9xcjai8sqi9xa3gvaifh8-zlib-1.2.12-dev
ZOUT=/nix/store/2yy85x1bhwmynzmpr4n29caxpfm0bkk4-zlib-1.2.12
export CABAL_DIR=/tmp/cabal-mgenv HOME=/tmp/cabal-mgenv
mkdir -p "$CABAL_DIR"
cd /tmp/mgenv
nix shell \
  nixpkgs/nixos-21.11#cabal-install nixpkgs/nixos-21.11#gmp nixpkgs/nixos-21.11#gmp.dev \
  nixpkgs/nixos-21.11#zlib nixpkgs/nixos-21.11#zlib.dev \
  nixpkgs/nixos-21.11#ncurses nixpkgs/nixos-21.11#pkg-config \
  --command bash -c "
    export PATH=$GHC865/bin:\$PATH
    export C_INCLUDE_PATH=$ZDEV/include:\${C_INCLUDE_PATH:-}
    export LIBRARY_PATH=$ZOUT/lib:$NUMALIB:\${LIBRARY_PATH:-}
    export PKG_CONFIG_PATH=$ZDEV/lib/pkgconfig:\${PKG_CONFIG_PATH:-}
    echo \"ghc: \$(ghc --version)\"
    cabal update 2>&1 | tail -1
    cabal build exe:mgenv-json -w $GHC865/bin/ghc \
      --extra-include-dirs=$ZDEV/include \
      --extra-lib-dirs=$ZOUT/lib --extra-lib-dirs=$NUMALIB 2>&1
    echo BUILD_EXIT=\$?
  "
echo SCRIPT_DONE
