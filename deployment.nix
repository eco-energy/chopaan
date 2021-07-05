let
  region = "ap-southeast-1";
  app = (import ./.) {};
  accessKeyId = "default";
  uijs = (import ./nix/snowman.nix).build { isJS = true; };
  janusPort = 8182;
  serverPort = 8080;
  mqttPort = 8883;
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
          elasticIPv4 = resources.elasticIPs.chopaan-ip;
        };

        route53 = {
          inherit accessKeyId region;
          hostName = "dosti.ecoenergy.global";
          usePublicDNSName = true;
        };
      };

      boot.loader.grub.device = lib.mkForce "/dev/nvme0n1";

      networking.firewall.allowedTCPPorts = [ 80 8093 ];

      docker-containers."janusgraph" = {
           image = "docker.io/janusgraph/janusgraph:latest";
           ports = [ "${toString janusPort}:${toString janusPort}" ];
           volumes = [
             "janusgraph-default-data:/var/lib/janusgraph"
             "./janusgraph-config/config/:/etc/opt/janusgraph:ro"
             "./janusgraph-config/indexes/net-spider-index.groovy:/files/net-spider-index.groovy"
                     ];
      };
      
      systemd.services.chopaan = {
        wantedBy = [ "multi-user.target" ];

        after = [ "docker-janusgraph.service" ];

        script =
          let
            chopaan = app.chopaan.kbtzim;
          in
            ''
            ${chopaan}/bin/kbtzim --tinkerHost "janusgraph" --tinkerPort ${toString janusPort}
            '';
      };

      
      systemd.services.server = {
        wantedBy = [ "multi-user.target" ];

        after = [ "docker-janusgraph.service" ];

        script =
          let
            server = app.chopaan.server;
          in
            ''
            ${server}/bin/server --assets ${uijs}/bin/ui.jsexe --port ${toString serverPort} --tinkerHost "janusgraph" --tinkerPort ${toString janusPort}
            '';
      };


      
      # systemd.services.ui = {
      #   wantedBy = [ "multi-user.target" ];

      #   after = [ "server.service" ];

      #   script =
      #     let
      #       ui = app.chopaan.ui;
      #       # --connectPort ${toString config.services.postgresql.port}
      #     in
      #       ''
      #       ${ui}/bin/ui
      #       '';
      # };
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
      elasticIPs.chopaan-ip = { inherit region accessKeyId; };
    };
  }
