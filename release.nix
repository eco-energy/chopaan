# release.nix
let
  chopaan = import(./default.nix);

  pkgsNative = import <nixpkgs> {};
  pkgsRaspberryPi = import <nixpkgs> {
    crossSystem = pkgsNative.lib.systems.examples.raspberryPi;
  };

  native = chopaan { pkgs = pkgsNative; };
  crossRaspberryPi = chopaan { pkgs = pkgsRaspberryPi; };

in {
  chopaan-native = native.chopaan.components.exes.chopaan;
  chopaan-raspberry-pi = crossRaspberryPi.chopaan.components.exes.chopaan;
}
