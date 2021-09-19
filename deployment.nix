let
  region = "ap-southeast-1";
  app = (import ./. {}).chopaan;
  pkgs = (import ./nix/default.nix {});
  accessKeyId = "default";
  ui = (import ./nix/snowman.nix).build { isJS = true; };
  staticUi = (import ./nix/website.nix) {};
  dashes = (import ./nix/dashboard.nix) {};
in
{
  network.description = "Chopaan and DB.";

  
  chopaan = { config, pkgs, resources, lib, ... }:
    let
      #uijs = "${staticUi}/bin/ui.jsexe";
      janusPort = 8182;
      serverPort = 8080;
      mqttPort = 8883;
      awskey = "/run/keys/aws-creds";
      tinkerHost = "localhost";
      janusConf = ./janusgraph-config;
      dnsName = "dosti.ecoenergy.global";
      chopaanDir = "/home/chopaan";
    in
     {
      deployment = {
        targetEnv = "ec2";
        
        ec2 = {
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

        route53 = {
          inherit accessKeyId region;
          hostName = config.services.grafana.domain;
          #hostName = dnsName;
          usePublicDNSName = true;
        };

        keys = { aws-creds = { text = builtins.readFile ./key; };
                 dosti-datastream = { text = builtins.readFile ./bucket-key; };
               };
      };

      environment.systemPackages = [ pkgs.z3 ];
      # nix.binaryCaches = lib.mkForce [
      #   "https://cache.nixos.org"
      #   "s3://ee-nixcache?region=ap-southeast-1"
      #   "https://pytorch-world.cachix.org"
      #   "https://hydra.iohk.io"
      #   "https://iohk.cachix.org"
      #   "https://nixcache.reflex-frp.org"
      #   "https://hasktorch.cachix.org"
      #   "https://shpadoinkle.cachix.org"
      # ];
      nix.trustedUsers = lib.mkForce ["root"];
      users.users = {
        chopaan = {
          createHome = true;
          group = "users";
          home = chopaanDir;
        };
      };
      # nix.binaryCachePublicKeys = lib.mkForce [
      #   "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      #   "ee-nixcache:qydUr3bm5mYfgWQDJn6S0VZGzGDZ5uwvzhEFlQVshDk="
      #   "pytorch-world.cachix.org-1:JCRxRpQ0JsP+a/GvSTmFDROLLd6rTmOpnm/gYqzS0KM="
      #   "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      #   "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
      #   "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
      #   "hasktorch.cachix.org-1:wLjNS6HuFVpmzbmv01lxwjdCOtWRD8pQVR3Zr/wVoQc="
      #   "shpadoinkle.cachix.org-1:aRltE7Yto3ArhZyVjsyqWh1hmcCf27pYSmO1dPaadZ8="
      # ];
      boot.loader.grub.device = lib.mkForce "/dev/nvme0n1";
      networking.firewall.enable = true;
      networking.firewall.allowedTCPPorts = [ 80 443 8883 ];
      environment.variables = { REGION = region; };
      security.pam.loginLimits = [
        { domain = "@root";
          item = "nproc";
          type = "soft";
          value = 1280000;
        }
        { domain = "@root";
          item = "nofile";
          type = "soft";
          value = 6400000;
        }
        
        
      ];

      # imports = 
      #   [ (
      #     (import ./nix/s3fs.nix { inherit pkgs lib; })
      #       { mount = "/mnt/dosti";
      #         bucket = "dosti-datastream";
      #       }
      #   )
      #   ];

      
      docker-containers."janusgraph" = {
           image = "docker.io/janusgraph/janusgraph:0.6.0";
           ports = [ "${toString janusPort}:${toString janusPort}" ];
           volumes = [
             "janusgraph-default-data:/var/lib/janusgraph"
             "${janusConf}/config/januskeyspaces.properties:/etc/opt/janusgraph/janusgraph.properties:ro"
             "${janusConf}/config/gremlin-server-0.6.yaml:/etc/opt/janusgraph/janusgraph-server.yaml:ro"
             "${janusConf}/indexes/net-spider-index.groovy:/files/net-spider-index.groovy"
             "${janusConf}/cassandra_truststore.jks:/opt/janusgraph/cassandra_truststore.jks"
                     ];
      };

      systemd.extraConfig = "DefaultLimitNOFILE=6400000";

      systemd.services.chopaan = {
        
        wantedBy = [ "multi-user.target" ];

        after = [ "network.target" "docker-janusgraph.service" ];
        environment = {
          AWS_CREDS = awskey;
          
        };
        path = [ pkgs.z3 ];
        #preStart = "mkdir -p ${cacheDir}";
        serviceConfig = {
          LimitNOFILE = 6400000;
          StateDirectory=chopaanDir;
          RuntimeDirectory=chopaanDir;
        };
        script =
          let
            chopaan = app.kbtzim;
          in
            ''
            ${chopaan}/bin/kbtzim --tinkerHost ${tinkerHost} --tinkerPort ${toString janusPort} +RTS -A32m -n4m -N
            '';
      };

      systemd.services.dashgen = {
        wantedBy = [ "grafana.service" ];
        after = [ "docker-janusgraph.service" "chopaan.service" ];
        script =
          let
            dashgen = app.dashgen;
          in
            ''
            ${dashgen}/bin/dashgen --tinkerHost ${tinkerHost} --tinkerPort ${toString janusPort} --outpath ${chopaanDir}
            '';
      };
      # systemd.services.server = {
      #   wantedBy = [ "multi-user.target" ];

      #   after = [ "docker-janusgraph.service" ];

      #   script =
      #     let
      #       server = app.server;
      #     in
      #       ''
      #       ${server}/bin/server --assets ${staticUi} --port ${toString serverPort} --tinkerHost ${tinkerHost} --tinkerPort ${toString janusPort}
      #       '';
      # };
      services.influxdb = {
        enable = true;
        extraConfig = {
          collectd = [{ enabled = false; }];
          udp = [{ enabled = true; }];
        };
      };
      services.grafana = {
        enable = true;
        domain = "dosti-monitor.ecoenergy.global";
        port = 2342;
        addr = "127.0.0.1";
        provision = {
          enable = true;
          dashboards = [
            { name = "Chopaan Dash";
              orgId = 1;
              type = "file";
              folder = "Chopaan";
              disableDeletion = false;
              updateIntervalSeconds = 30;
              options.path = dashes;
            }
          ];
          datasources = [
            { name = "InfluxDB";
              type = "influxdb";
              access = "proxy";
              orgId = 1;
              url = "http://localhost:8086";
              editable = false;
            }
          ];
          
        };
      };
      users.users.nginx.extraGroups = [ "acme" ];
      security.acme.acceptTerms = true;
      security.acme.email = "faez@ecoenergy.global";
      security.acme.server = "https://acme-staging-v02.api.letsencrypt.org/directory";
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

        virtualHosts.${config.services.grafana.domain} = {
          forceSSL = true;
          enableACME = true; 
          locations."/" = {
            proxyPass = "http://127.0.0.1:${toString config.services.grafana.port}";
            proxyWebsockets = true;
            extraConfig =
              # required when the target is also TLS server with multiple hosts
              "proxy_ssl_server_name on;" +
              # required when the server wants to use HTTP Authentication
              "proxy_pass_header Authorization;"
            ;
          };
        };
        # virtualHosts.${dnsName} = {
        #   forceSSL = true;
        #   enableACME = true;
        #   locations."/" = {
        #     proxyPass = "http://127.0.0.1:${toString serverPort}";
        #     root = staticUi;
        #   };
        #   #extraConfig = ""
        #   locations."~* .(jpe?g|svg|png|gif|ico|css|js|webmanifest|json|fbx)$" = {
        #     root = staticUi;
        #     extraConfig = "proxy_cache chop-cache;";
        #     tryFiles = "$uri uri/ =404";
        #   };
        # };
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
        "mqtt" = {
          inherit accessKeyId region;

          rules = [
            { fromPort = 8883; toPort = 8883; sourceIp = "0.0.0.0/0"; }
          ];
        };
      };
      
      elasticIPs.chopaan-ip = { inherit region accessKeyId; };

      iamRoles.chopaan-role = { inherit region accessKeyId;
                                name = "chopaanRole";
                                assumeRolePolicy = ''
                                {
                                  "Version": "2012-10-17",
                                  "Statement": [
                                      {
                                        "Sid": "",
                                        "Effect": "Allow",
                                        "Principal": {
                                          "Service": "ec2.amazonaws.com"
                                        },
                                        "Action": "sts:AssumeRole"
                                      }
                                  ]
                                }
                                '';
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
                                           "Action": [
                                               "cassandra:*"
                                           ],
                                           "Resource": [
                                               "*"
                                           ]
                                       }
                                   ]
                                }
                                '';
                              };
    };
  }
