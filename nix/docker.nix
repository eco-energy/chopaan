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
  exes = c.chopaan;
in
{
  server = pkgs.dockerTools.buildImage {
    name = "chopaan-server";
    tag = "latest";
    contents = [ exes.server pkgs.iana-etc pkgs.cacert ];
    created = "now";
    config = {
      Cmd = [ "${exes.server}/bin/server" ];
      Version = "1.0";
      ExposedPorts = {
        "8883/tcp" = {};
      };
    };
  };

  ui = pkgs.dockerTools.buildImage {
    name = "chopaan-ui";
    tag = "latest";
    contents = [ exes.ui pkgs.iana-etc pkgs.cacert ];
    created = "now";
    config = {
      Cmd = [ "${exes.ui}/bin/ui" ];
      Version = "1.0";
      ExposedPorts = {
        "8884/tcp" = {};
      };
    };
  };

  kbtzim = pkgs.dockerTools.buildImage {
    name = "chopaan-kbtzim";
    tag = "latest";
    contents = [ exes.kbtzim pkgs.iana-etc pkgs.cacert ];
    created = "now";
    config = {
      Cmd = [ "${exes.kbtzim}/bin/kbtzim" ];
      Version = "1.0";
      ExposedPorts = {
        "8885/tcp" = {};
      };
    };
  };
}
