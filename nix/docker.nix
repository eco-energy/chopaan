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
  ui = (import ./snowman.nix).build { isJS = true; };
  uijs = "${ui}/bin/ui.jsexe/";
  tinkerHost = "db";
  janusPort = 8182;
  
  alp = pkgs.dockerTools.pullImage {
      imageName = "alpine";
      imageDigest = "sha256:52a197664c8ed0b4be6d3b8372f1d21f3204822ba432583644c9ce07f7d6448f";
      sha256 = "sha256:1n589h9sg4lpxi0bmbh4ba682w6551jicx3zm5qph2lc0p2c5rb3";
  };

  chopaanImage = svcName: exe: port: pkgs.dockerTools.buildImage {
    name = "chopaan-${svcName}";
    tag = "latest";
    fromImage = alp;
    contents = [ exe pkgs.iana-etc pkgs.cacert ];
    created = "now";
    config = {
      Cmd = [
        "${exe}/bin/${svcName} --tinkerHost ${tinkerHost} --tinkerPort ${toString janusPort}"
      ];
      Version = "1.0";
      ExposedPorts = {
        "${toString port}/tcp" = {};
      };
    };
  };
  
  svrImg = svcName: exe: assets: port: pkgs.dockerTools.buildImage {
    name = "chopaan-${svcName}";
    tag = "latest";
    fromImage = alp;
    contents = [ exe assets pkgs.iana-etc pkgs.cacert ];
    created = "now";
    config = {
      Cmd = [
        "${exe}/bin/${svcName} --port ${toString port} --assets ${assets} --tinkerHost ${tinkerHost} --tinkerPort ${toString janusPort}"
      ];
      Version = "1.0";
      ExposedPorts = {
        "${toString port}/tcp" = {};
      };
    };
  };
  
in
{
  kbtzim = chopaanImage "kbtzim" exes.kbtzim 8888;
  server = svrImg "server" exes.server uijs 8080;
}
