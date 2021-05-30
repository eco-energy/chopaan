let
  region = "ap-southeast-1";

  accessKeyId = "default";

in
{
  network.description = "Chopaan and DB.";
  
  machine = { config, pkgs, resources, ... }: {
      deployment = {
        targetEnv = "ec2";
        
        ec2 = {
          inherit accessKeyId region;

          instanceType = "t3.nano";

          ebsBoot = true;
          ebsInitialRootDiskSize = 10;

          keyPair = resources.ec2KeyPairs.chopaan-key-pair;

          securityGroups = [
            resources.ec2SecurityGroups."http"
            resources.ec2SecurityGroups."ssh"
          ];
        };
      };
      #fileSystems."/" =
      #  { autoFormat = true;
      #    fsType = "btrfs";
      #    device = "/dev/nvme1n1";
      #    ec2.size = 10;
      #    ec2.volumeType = "gp2";
      #  };

      networking.firewall.allowedTCPPorts = [ 80 8093 ];

      # services.postgresql = {
      #   enable = true;
      #   extraPlugins = [ pkgs.timescaledb ];
      #   settings = { shared_preload_libraries = "timescaledb"; };
      #   authentication = ''
      #     local all all ident map=mapping
      #   '';

      #   identMap = ''
      #     mapping root     postgres
      #     mapping postgres postgres
      #   '';

      #   package = pkgs.postgresql_11;

      #   initialScript = ./db_migrations/1.psql;
      # };
      docker-containers."janusgraph" = {
        image = "docker.io/janusgraph/janusgraph:latest";
        ports = [ "8182:8182" ];
      };
      systemd.services.chopaan = {
        wantedBy = [ "multi-user.target" ];

        after = [ "postgresql.service" ];

        script =
          let
            app = (import ./.) {};
            chopaan-server = app.chopaan.components.exes.server;
            #chopaan-frontend = app.chopaan.components.exes.ui
            # --connectPort ${toString config.services.postgresql.port}
          in
            ''
            ${chopaan-server}/bin/server
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
