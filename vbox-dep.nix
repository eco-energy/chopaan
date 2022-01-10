{
  chopaan = { config, pkgs, resources, lib, app, sops-nix, ... }:
    {
      imports = [
        ( import ./deploy/machine.nix {
          inherit config pkgs resources lib app sops-nix;
          hostName = "localhost";
          grubDevice = "/dev/sda";
        } )
      ];
      deployment = {
        targetEnv = "none"; # virtualbox
        # virtualbox.memorySize = 4096;
        # virtualbox.vcpu = 4;
        # virtualbox.headless = true;

        #keys = {
        #  aws-creds = {
        #    text = builtins.readFile ./credentials/key;
        #  };
        #  dosti-datastream = {
        #    text = builtins.readFile ./credentials/dost-datastream-key;
        #  };
        #};
      };
  };
}
