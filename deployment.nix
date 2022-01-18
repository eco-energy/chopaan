let
  region = "ap-southeast-1";
  zone = "${region}a";
  accessKeyId = "default";
  hostName = "dosti.ecoenergy.global";
  grubDevice = "/dev/nvme1n1";
  in
  {
    chopaan = { config, pkgs, resources, lib, chopaan, sops-nix, ... }:
    {
      imports = [
        ( import ./deploy/machine.nix {
          inherit config pkgs resources lib hostName grubDevice chopaan sops-nix;
        })
      ];
      deployment.targetEnv = "ec2";

      deployment.ec2 = {
        inherit accessKeyId region zone;

        instanceType = "m5n.xlarge";
        ebsOptimized = true;
        spotInstancePrice = 10;
        spotInstanceRequestType = "persistent";
        spotInstanceInterruptionBehavior = "stop";
        ebsBoot = true;
        ebsInitialRootDiskSize = 100;
        keyPair = resources.ec2KeyPairs.chopaan-key-pair;
        instanceProfile = resources.iamRoles.chopaan-role.name;
        securityGroups = [
          resources.ec2SecurityGroups."http"
          resources.ec2SecurityGroups."https"
          resources.ec2SecurityGroups."ssh"
        ];
        associatePublicIpAddress = true;
        elasticIPv4 = resources.elasticIPs.chopaan-ip;
      };
      deployment.route53 = {
        inherit accessKeyId;
        hostName = hostName;
        usePublicDNSName = true;
      };
      #fileSystems.disk = resources.ebsVolumes.chopaanFS.volumeId; 
      fileSystems.chopaan = {
        mountPoint = "/chopaanFS";
        device = grubDevice;
        fsType = "btrfs";
        #autoFormat = true;
        ec2 = {
          disk = resources.ebsVolumes.chopaanFS;
          fsType = "btrfs";
          deleteOnTermination = false;
        }; 
      };
    };

    resources.ebsVolumes.chopaanFS = { inherit region accessKeyId zone;
                                       #volumeId = "vol-054f45b750609bba5";
                                       size = 120;
                                       volumeType = "gp2";
                                       deleteOnTermination = false;
                                     };
    resources.ec2KeyPairs.chopaan-key-pair = { inherit region accessKeyId; };

    resources.ec2SecurityGroups = {
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
    
    resources.elasticIPs.chopaan-ip = { inherit region accessKeyId; };
    resources.iamRoles.chopaan-role = {
      inherit region accessKeyId;
      name = "chopaanRole";
      assumeRolePolicy = builtins.readFile ./aws/assumeRole.json;
      policy = builtins.readFile ./aws/chopaan-role.json;
    };
    
    
  }

