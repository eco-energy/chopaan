{ compiler ? "ghc865"
}:
let  
  chan = (import shpadoink + "/nix/chan.nix")
  inherit (import shpadoink + "/nix/util.nix" { inherit compiler; isJS = true; pkgs = pkgsJS; }) compilerjs;
  pkgsJS  = import (./snowman.nix) { inherit compiler; isJS = true; };
  pkgsGHC = import (./snowman.nix) { inherit compiler; isJS = false; };
  client  = pkgsJS.haskell.packages.${compilerjs}.chopaan + "/bin/ui.jsexe/";
  server  = pkgsGHC.haskell.packages.${compiler}.chopaan  + "/bin/server";
in import shpadoink + "/nix/docker.nix" {
  inherit client server;
  pkgs      = pkgsGHC;
  imgName   = "chopaan-crud-docker";
  extraArgs = "--assets ${client}";
}
