let
  region = "ap-southeast-1";
  app = (import ./. {}).chopaan;
  pkgs = (import ./nix/default.nix {});
  accessKeyId = "default";
  ui = (import ./nix/snowman.nix).build { isJS = true; };
  staticUi = (import ./nix/website.nix) {};
in
{
  network.description = "Chopaan and DB.";

  
  chopaan = { config, pkgs, resources, lib, ... }:
    let
      uijs = "${staticUi}/bin/ui.jsexe";
      janusPort = 8182;
      serverPort = 8080;
      mqttPort = 8883;
      tinkerHost = "localhost";
      janusConf = ./janusgraph-config;
      dnsName = "dosti.ecoenergy.global";
    in
     {
      deployment = {
        targetEnv = "ec2";
        
        ec2 = {
          inherit accessKeyId region;

          instanceType = "t3.medium";

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

        route53 = {
          inherit accessKeyId region;
          hostName = dnsName;
          usePublicDNSName = true;
        };

        keys.aws-creds = { text = builtins.readFile ./key;
                   };
      };
      
      boot.loader.grub.device = lib.mkForce "/dev/nvme0n1";
      networking.firewall.enable = true;
      networking.firewall.allowedTCPPorts = [ 80 443 ];
      environment.systemPackages = [ pkgs.z3 ];
      environment.variables = { SERVER_HOST = dnsName;
                                SERVER_PORT = "443";
                                REGION = region;
                              };
      
      docker-containers."janusgraph" = {
           image = "docker.io/janusgraph/janusgraph:latest";
           ports = [ "${toString janusPort}:${toString janusPort}" ];
           volumes = [
             "janusgraph-default-data:/var/lib/janusgraph"
             "${janusConf}/config:/etc/opt/janusgraph:ro"
             "${janusConf}/indexes/net-spider-index.groovy:/files/net-spider-index.groovy"
                     ];
      };
      
      systemd.services.chopaan = {
        wantedBy = [ "multi-user.target" ];

        after = [ "network.target" "docker-janusgraph.service" ];
        environment = {
          HOME = "/root";
          #PATH = "${pkgs.z3}/lib";
        };
        script =
          let
            chopaan = app.kbtzim;
          in
            ''
            ${chopaan}/bin/kbtzim --tinkerHost ${tinkerHost} --tinkerPort ${toString janusPort}
            '';
      };

      
      systemd.services.server = {
        wantedBy = [ "multi-user.target" ];

        after = [ "docker-janusgraph.service" ];

        script =
          let
            server = app.server;
          in
            ''
            ${server}/bin/server --assets ${staticUi} --port ${toString serverPort} --tinkerHost ${tinkerHost} --tinkerPort ${toString janusPort}
            '';
      };

      users.users.nginx.extraGroups = [ "acme" ];
      security.acme.acceptTerms = true;
      security.acme.email = "faez@ecoenergy.global";
      services.nginx = {
        enable = true;
        logError = "stdout info";
        recommendedTlsSettings = true;
        recommendedOptimisation = true;
        recommendedGzipSettings = true;
        recommendedProxySettings = true;
        appendHttpConfig = ''
        proxy_cache_path /tmp/cache/ levels=1:2 keys_zone=chop-cache:100m max_size=10g inactive=60m use_temp_path=off;
        # Cache only success status codes; in particular we don't want to cache 404s.
        # See https://serverfault.com/a/690258/128321
        map $status $cache_header {
          200     "public";
          302     "public";
          default "no-cache";
        }
        access_log logs/access.log;
      '';
        
        virtualHosts.${dnsName} = {
          forceSSL = true;
          enableACME = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:${toString serverPort}";
            root = staticUi;
          };
          #extraConfig = ""
          locations."~* .(jpe?g|svg|png|gif|ico|css|js|webmanifest|json|fbx)$" = {
            root = staticUi;
            extraConfig = "proxy_cache chop-cache;";
            tryFiles = "$uri uri/ =404";
          };
        };
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
                                policy = ''
                                {
                                   "Version": "2012-10-17",
                                   "Statement": [
                                       {
                                           "Effect": "Allow",
                                           "Action": [
                                               "iot:*"
                                           ],
                                           "Resource": "*"
                                       },
                                       {
                                           "Effect": "Allow",
                                           "Action": "s3:*",
                                           "Resource": "*"
                                       },
                                       {
                                           "Effect": "Allow",
                                           "Action": "ec2:*",
                                           "Resource": "*"
                                       }
                                   ]
                                }
                                '';
                              };
    };
  }
