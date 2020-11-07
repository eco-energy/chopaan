{
  sources ? import (./sources.nix)
, pkgs ? import sources.nixpkgs {}
, chopaan ? import (../default.nix) {} 
}:

pkgs.dockerTools.buildImage {
    name = "chopaan-0";
    contents = [ chopaan.chopaan.components.exes.chopaan-exe ];
    
    config = {
      Cmd = [ "chopaan-exe" ];
      ExposedPorts = {
        "8883/tcp" = {};
      };
    };
}
