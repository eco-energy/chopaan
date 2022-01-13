{ config, pkgs, lib, ... }:
{
  sops = {
      defaultSopsFile = ../credentials/chopaan.yaml;
      age.generateKey = false; #[ "" ];
      age.keyFile = "/var/lib/sops-nix/key.txt";
      age.sshKeyPaths = lib.mkForce []; #[ "/etc/ssh/ssh_host_ed25519_key" ];
      validateSopsFiles = true;
      secrets.aws-creds = {
        name = "aws-creds";
        key = "aws-creds";
        owner = "chopaan";
        group = "chopaan";
        mode = "0440";
        neededForUsers = false;
        sopsFile = ../credentials/chopaan.yaml;
        restartUnits = [ "chopaan.service" ];
      };
      keepGenerations = 1;
      log = [ "keyImport" "secretChanges"];
  };
    #opts' = builtins.mkMerge [ config.sops opts ];
}
