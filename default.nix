let
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {};
  nixpkgsSrc = haskellNix.sources.nixpkgs-2003;
  nixpkgsArgs = haskellNix.nixpkgsArgs;
in
{ pkgs ? import nixpkgsSrc nixpkgsArgs
}:
let
  pkgSet = pkgs.haskell-nix.mkStackPkgSet {
    stack-pkgs = import ./nix/pkgs.nix;
    pkg-def-extras = [];
    modules = [];
  };

in
pkgSet.config.hsPkgs // { _config = pkgSet.config; }
