let
  region = "ap-southeast-1";
  pkgs = (import ./nix/default.nix {});
  accessKeyId = "default";
  hostName = "dosti.ecoenergy.global";
  grubDevice = "/dev/nvme0n1";
in
{
  network.description = "Chopaan Services And Data Stores.";
  #chopaan.deployment 
  # = aws;
  chopaan = { config, pkgs, resources, lib, ... }:
  {
    imports = [
      ( import ./deploy/machine.nix { inherit config pkgs resources lib hostName grubDevice; })
    ];
    deployment.targetEnv = "ec2";

    deployment.ec2 = {
      inherit accessKeyId region;

      instanceType = "m6i.large";
      spotInstancePrice = 04;
      ebsBoot = true;
      ebsInitialRootDiskSize = 100;

      keyPair = resources.ec2KeyPairs.chopaan-key-pair;
      instanceProfile = resources.iamRoles.chopaan-role.name;
      securityGroups = [
        resources.ec2SecurityGroups."http"
        resources.ec2SecurityGroups."https"
        resources.ec2SecurityGroups."ssh"
      ];
      elasticIPv4 = resources.elasticIPs.chopaan-ip;
    };
    deployment.route53 = {
      inherit accessKeyId region;
      hostName = hostName;
      usePublicDNSName = true;
    };
  };


resources = {
  ec2KeyPairs.chopaan-key-pair = { inherit region accessKeyId; };

  ec2SecurityGroups = {
    "http" = {
      inherit accessKeyId region;

      rules = [
        { fromPort = 80; toPort = 80; sourceIp = "0.0.0.0/0"; }
      ];
    };

    "https" = {
      inherit accessKeyId region;

      rules = [
        { fromPort = 443; toPort = 443; sourceIp = "0.0.0.0/0"; }
      ];
    };

    "ssh" = {
      inherit accessKeyId region;

      rules = [
        { fromPort = 22; toPort = 22; sourceIp = "0.0.0.0/0"; }
      ];
    };
  };

  elasticIPs.chopaan-ip = { inherit region accessKeyId; };

  iamRoles.chopaan-role = { inherit region accessKeyId;
  name = "chopaanRole";
  assumeRolePolicy = builtins.readFile ./aws/assumeRole.json;
  policy = builtins.readFile ./aws/chopaan-role.json;
  };
};
}
