{ pkgs ? import ./nix/default.nix {}
, hostName ? "localhost"
}:

{
  network.description = "Chopaan Development Environment.";
  
  chopaan = { config, pkgs, resources, lib, ... }:
    {
      imports = [
        ( import ./machine.nix {
          inherit config pkgs resources lib hostName;
          grubDevice = "/dev/sda";
        } )
      ];
      deployment = {
        targetEnv = "virtualbox";
        virtualbox.memorySize = 4096;
        virtualbox.vcpu = 4;
        virtualbox.headless = true;

        keys = {
          aws-creds = {
            text = builtins.readFile ./credentials/key;
          };
          dosti-datastream = {
            text = builtins.readFile ./credentials/dost-datastream-key;
          };
        };
      };
  };
}
