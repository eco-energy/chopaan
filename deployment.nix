let
  region = "ap-southeast-1";

  accessKeyId = "default";

in
  { machine = { config, pkgs, resources, ... }: {
      deployment = {
        targetEnv = "ec2";
        #targetEnv = "virtualbox";
        #virtualbox.headless = true;
        #virtualbox.memorySize = 1024;
        #virtualbox.vcpu = 1;
        ec2 = {
          inherit accessKeyId region;

          instanceType = "t3.nano";

          keyPair = resources.ec2KeyPairs.chopaan-key-pair;

          securityGroups = [
            resources.ec2SecurityGroups."http"
            resources.ec2SecurityGroups."ssh"
          ];
        };
      };

      networking.firewall.allowedTCPPorts = [ 80 ];

      services.postgresql = {
        enable = true;
        extraPlugins = [ pkgs.timescaledb ];
        extraConfig = "shared_preload_libraries = 'timescaledb'";
        authentication = ''
          local all all ident map=mapping
        '';

        identMap = ''
          mapping root     postgres
          mapping postgres postgres
        '';

        package = pkgs.postgresql_11;

        initialScript = ./db_migrations/1.psql;
      };

      systemd.services.chopaan = {
        wantedBy = [ "multi-user.target" ];

        after = [ "postgresql.service" ];

        script =
          let
            app = (import ./.) {};
            chopaan-exe = app.chopaan.components.exes.chopaan-exe;
            # --connectPort ${toString config.services.postgresql.port}
          in
            ''
            ${chopaan-exe}
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
