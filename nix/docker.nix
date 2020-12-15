{ # Fetch the latest haskell.nix and import its default.nix
  sources ? import ./sources.nix
, haskellNix ? import sources."haskell.nix" {}
# haskell.nix provides access to the nixpkgs pins which are used by our CI,
# hence you will be more likely to get cache hits when using these.
# But you can also just use your own, e.g. '<nixpkgs>'.
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2003

# haskell.nix provides some arguments to be passed to nixpkgs, including some
# patches and also the haskell.nix functionality itself as an overlay.
, nixpkgsArgs ? haskellNix.nixpkgsArgs

# import nixpkgs with overlays
, pkgs ? import nixpkgsSrc nixpkgsArgs

, c ? import (../default.nix) {} 
}:


let
  chopaan = c.chopaan.components.exes.chopaan-exe;
in
pkgs.dockerTools.buildImage {
  name = "chopaan";
  tag = "latest";
  contents = [ chopaan pkgs.iana-etc pkgs.cacert ];
    
  config = {
    Cmd = [ "${chopaan}/bin/chopaan-exe" ];
    Version = "1.0";
    ExposedPorts = {
      "8883/tcp" = {};
    };
  };
}
