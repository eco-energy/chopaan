let
  region = "ap-southeast-1";
  app = (import ./.) {};
  accessKeyId = "default";

in
{
  network.description = "Chopaan and DB.";

  
  machine = { config, pkgs, resources, lib, ... }: {
      deployment = {
        targetEnv = "ec2";
        
        ec2 = {
          inherit accessKeyId region;

          instanceType = "t3.nano";

          ebsBoot = true;
          ebsInitialRootDiskSize = 100;

          keyPair = resources.ec2KeyPairs.chopaan-key-pair;

          securityGroups = [
            resources.ec2SecurityGroups."http"
            resources.ec2SecurityGroups."ssh"
          ];
        };
      };

      boot.loader.grub.device = lib.mkForce "/dev/nvme0n1";

      networking.firewall.allowedTCPPorts = [ 80 8093 ];

      docker-containers."janusgraph" = {
           image = "docker.io/janusgraph/janusgraph:latest";
           ports = [ "8182:8182" ];
      };
      
      systemd.services.chopaan = {
        wantedBy = [ "multi-user.target" ];

        after = [ "docker-janusgraph.service" ];

        script =
          let
            chopaan = app.chopaan.kbtzim;
          in
            ''
            ${chopaan}/bin/kbtzim
            '';
      };

      
      systemd.services.server = {
        wantedBy = [ "multi-user.target" ];

        after = [ "docker-janusgraph.service" ];

        script =
          let
            server = app.chopaan.server;
            # --connectPort ${toString config.services.postgresql.port}
          in
            ''
            ${server}/bin/server
            '';
      };


      
      systemd.services.ui = {
        wantedBy = [ "multi-user.target" ];

        after = [ "server.service" ];

        script =
          let
            ui = app.chopaan.ui;
            # --connectPort ${toString config.services.postgresql.port}
          in
            ''
            ${ui}/bin/ui
            '';
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

        "ssh" = {
          inherit accessKeyId region;

          rules = [
            { fromPort = 22; toPort = 22; sourceIp = "0.0.0.0/0"; }
          ];
        };
      };
    };
  }
