# release.nix
let
  sources = import ./nix/sources.nix;
  haskellNix = import sources."haskell.nix" {};
  nixpkgsSrc = haskellNix.sources.nixpkgs-2003;
  nixpkgsArgs = haskellNix.nixpkgsArgs;
  # import nixpkgs with overlays
  pkgs = import nixpkgsSrc nixpkgsArgs;

  chopaan = import(./default.nix);
  nativeArgs = nixpkgsArgs // {
    crossSystem = pkgsNative.lib.systems.examples.raspberryPi;
  };
  pkgsNative = pkgs;
  pkgsRaspberryPi = import nixpkgsSrc nativeArgs; 

  native = chopaan { pkgs = pkgsNative; };
  crossRaspberryPi = chopaan { pkgs = pkgsRaspberryPi; };

in {
  chopaan-native = native.chopaan.components.exes.chopaan-exe;
  chopaan-raspberry-pi = crossRaspberryPi.chopaan.components.exes.chopaan-exe;
}
