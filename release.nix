# release.nix
let
  sources = import ./nix/sources.nix {};
  haskellNix = import sources."haskell.nix" {};
  nixpkgsSrc = haskellNix.sources.nixpkgs-2003;
  nixpkgsArgs = haskellNix.nixpkgsArgs;
  # import nixpkgs with overlays
  pkgs = import nixpkgsSrc nixpkgsArgs;

  chopaan = import(./default.nix);
  raspiArgs = nixpkgsArgs // {
    crossSystem = pkgs.lib.systems.examples.raspberryPi;
  };
  #ghcjsArgs = nixpkgsArgs // {
  #  crossSystem = pkgs.lib.systems.examples.ghcjs;
  #};
  pkgsNative = pkgs;
  pkgsRaspberryPi = import nixpkgsSrc raspiArgs; 
  #pkgsGhcjs = import nixpkgsSrc ghcjsArgs;
  native = chopaan { pkgs = pkgsNative; };
  crossRaspberryPi = chopaan { pkgs = pkgsRaspberryPi; };
  #crossGhcjs = chopaan { pkgs = pkgsGhcjs; };
in {
  chopaan-native = native.chopaan.components.exes.chopaan-exe;
  chopaan-raspberry-pi = crossRaspberryPi.chopaan.components.exes.chopaan-exe;
  #chopaan-ghcjs = crossGhcjs.chopaan.components.exes.ui;
}
