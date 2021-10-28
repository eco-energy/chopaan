# release.nix
let
  sources = import ./nix/sources.nix {};
  haskellNix = import sources."haskell.nix" {};
  nixpkgsSrc = haskellNix.sources.nixpkgs-2009;
  nixpkgsArgs = haskellNix.nixpkgsArgs;
  # import nixpkgs with overlays
  pkgs = import nixpkgsSrc nixpkgsArgs;

  raspi = import(./default.nix) {
    crossSystem = pkgs.lib.systems.examples.raspberryPi;
  };
  native = (import ./default.nix) {};
  #crossGhcjs = chopaan { pkgs = pkgsGhcjs; };
in {
  # inherit native raspi;
  kbtzim-native = native.chopaan.kbtzim;
  kbtzim-raspi = raspi.chopaan.kbtzim;
  #chopaan-ghcjs = crossGhcjs.chopaan.components.exes.ui;
}
