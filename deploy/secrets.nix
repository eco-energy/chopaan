{ config, pkgs, lib, ... }:
{
  sops = {
      defaultSopsFile = ../credentials/chopaan.yaml;
      age.generateKey = true; #[ "" ];
      age.keyFile = "/home/faezs/.config/sops/age/keys.txt"; #"/var/lib/sops-nix/key.txt";
      age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
      validateSopsFiles = true;
      secrets.aws-creds = {
        owner = "chopaan";
        group = "chopaan";
        mode = "0440";
        neededForUsers = false;
        sopsFile = ../credentials/chopaan.yaml;
      };
      keepGenerations = 1;
      log = [ "keyImport" "secretChanges"];
  };
    #opts' = builtins.mkMerge [ config.sops opts ];
}
