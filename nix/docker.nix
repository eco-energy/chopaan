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
  uiJS = import (./snowman.nix) { isJS = true; }
  
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
      Cmd = [ "${exe}/bin/${svcName}" ];
      Version = "1.0";
      ExposedPorts = {
        "${toString port}/tcp" = {};
      };
    };
  };
  
in

{
  kbtzim = chopaanImage "kbtzim" exes.kbtzim 9999;
  server = chopaanImage "server" exes.server 8888;
  ui = chopaanImage "ui" exes.ui 8080;
}
