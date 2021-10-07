let
  region = "ap-southeast-1";
  pkgs = (import ./nix/default.nix {});
  accessKeyId = "default";
in
{
  network.description = "Chopaan Services And Data Stores.";
  
  chopaan = { config, pkgs, resources, lib, ... }:
    let
      #uijs = "${staticUi}/bin/ui.jsexe";
      app = (import ./. {}).chopaan;
      janusPort = 8182;
      serverPort = 8080;
      mqttPort = 8883;
      awskey = "/run/keys/aws-creds";
      tinkerHost = "localhost";
      janusConf = ./janusgraph-config;
      dnsName = "dosti.ecoenergy.global";
      frontend = (import ./nix/website.nix) {};
      #dashes = (import ./nix/dashboard.nix) {};
      withJanus = p: "${p} --tinkerHost ${tinkerHost} --tinkerPort ${toString janusPort}";
      withRTSOpts = p: "${p} +RTS -A32m -n4m -N";
      dashboardDir = "/dash";
      chopaanDir = "${config.users.users.chopaan.home}";
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
          hostName = dnsName;
          usePublicDNSName = true;
        };

        keys = { aws-creds = { text = builtins.readFile ./key; };
                 dosti-datastream = { text = builtins.readFile ./bucket-key; };
               };
      };

      environment.systemPackages = [ pkgs.z3 ];
      nix.trustedUsers = lib.mkForce ["root" ];
      users = {
        users = {
          chopaan = {
            createHome = true;
            group = "chopaan";
            extraGroups = ["keys" "root"];
            home = "/chopaan";
            useDefaultShell = true;
          };
        };
        groups.chopaan = {};
      };
      
      boot.loader.grub.device = lib.mkForce "/dev/nvme0n1";
      networking.firewall.enable = true;
      networking.firewall.allowedTCPPorts = [ 80 443 ];
      environment.variables = { REGION = region; };
      security.pam.loginLimits = [
        { domain = "@chopaan";
          item = "nproc";
          type = "soft";
          value = 1280000;
        }
        { domain = "@chopaan";
          item = "nofile";
          type = "soft";
          value = 6400000;
        }
      ];

      
      docker-containers.janusgraph = {
        image = "docker.io/janusgraph/janusgraph:0.6.0";
        ports = [ "${toString janusPort}:${toString janusPort}" ];
        volumes = [
          "janusgraph-default-data:/var/lib/janusgraph"
          "${janusConf}/config/janusgraph.properties:/etc/opt/janusgraph/janusgraph.properties:ro"
          "${janusConf}/config/gremlin-server-0.6.yaml:/etc/opt/janusgraph/janusgraph-server.yaml:ro"
          "${janusConf}/indexes/net-spider-index.groovy:/files/net-spider-index.groovy"
          # "${janusConf}/cassandra_truststore.jks:/opt/janusgraph/cassandra_truststore.jks"
        ];
      };

      docker-containers.chronograf = {
        image = "docker.io/chronograf:1.9.0-alpine";
        ports = [ "8888:8888" ];
        cmd = [ "--influxdb-url=http://localhost:8086" ];
        extraDockerOptions = [ "--network=host" ];
      };
      
      systemd.extraConfig = "DefaultLimitNOFILE=6400000\nDefaultStandardError='journal'\nDefaultStandardOut='journal'";
 
      systemd.services.chopaan = {
        after = [ "network.target" "docker-janusgraph.service" ];        
        wantedBy = [ "multi-user.target" ];

        environment = {
          AWS_CREDS = awskey;
          XDG_ROOT_DIR = chopaanDir;
        };
        
        path = [ pkgs.z3 ];

        serviceConfig = {
          # WorkingDirectory = "~";
          # User = "chopaan";
          # Group = "chopaan";
          LimitNOFILE = 6400000;
          # CacheDirectory = "chopaan";
          # CacheDirectoryMode = "0770";
        };
        
        unitConfig.RequiresMountsFor = chopaanDir;
        script = withRTSOpts ((withJanus "${app.kbtzim}/bin/kbtzim"));
      };

      systemd.services.hydrate = {
        
        wantedBy = [ "multi-user.target" ];

        after = [ "network.target" "docker-janusgraph.service" "influxdb.service" ];

        environment = {
          AWS_CREDS = awskey;
          XDG_ROOT_DIR = chopaanDir;
        };

        serviceConfig = {
          # WorkingDirectory = "~";
          # User = "";
          # Group = "chopaan";
          LimitNOFILE = 6400000;
          # CacheDirectory = "hydrate";
          # CacheDirectoryMode = "0770";
        };
        unitConfig.RequiresMountsFor = chopaanDir;
        
        script = withRTSOpts ((withJanus "${app.hydrate}/bin/hydrate"));
      };
      
      systemd.services.dashgen = {
        wantedBy = [ "grafana.service" ];
        after = [ "docker-janusgraph.service" "chopaan.service" ];
        # serviceConfig = {
        #   WorkingDirectory = "~";
        #   User = "chopaan";
        #   Group = "chopaan";
        #   CacheDirectory = "dash";
        #   CacheDirectoryMode = "0770";
        # };
        preStart = ''
        mkdir -p ${dashboardDir}
        chmod -R 644 ${dashboardDir}
        '';
        unitConfig.RequiresMountsFor = dashboardDir;
        script = (withJanus "${app.dashgen}/bin/dashgen --outpath ${dashboardDir}");
      };
      
      # systemd.services.server = {
      #   wantedBy = [ "multi-user.target" ];

      #   after = [ "docker-janusgraph.service" ];

      #   script = withRTSOpts (withJanus "${app.server}/bin/server --assets ${frontend} --port ${toString serverPort}");
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
        domain = dnsName;
        #rootUrl = "https://dosti.ecoenergy.global";
        port = 2342;
        addr = "127.0.0.1";
        #extraOptions = { SERVE_FROM_SUB_PATH= "true"; };
        provision = {
          enable = true;
          dashboards = [
            { name = "Chopaan Dash";
              orgId = 1;
              type = "file";
              folder = "Chopaan";
              disableDeletion = false;
              updateIntervalSeconds = 30;
              options.path = "${dashboardDir}";
            }
          ];
          datasources = [
            { name = "InfluxDB";
              type = "influxdb";
              access = "proxy";
              orgId = 1;
              url = "http://localhost:8086";
              editable = true;
            }
          ];  
        };
      };
      
      users.users.nginx.extraGroups = [ "acme" ];
      security.acme.acceptTerms = true;
      security.acme.email = "faez@ecoenergy.global";
      #security.acme.server = "https://acme-staging-v02.api.letsencrypt.org/directory";
      services.nginx = {
        enable = true;
        logError = "stdout info";
        recommendedTlsSettings = true;
        recommendedOptimisation = true;
        recommendedGzipSettings = true;
        recommendedProxySettings = true;
        appendHttpConfig = ''
        proxy_cache_path /tmp/cache/ levels=1:2 keys_zone=chop-cache:100m max_size=1g inactive=60m use_temp_path=off;
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
            proxyPass = "http://127.0.0.1:${toString config.services.grafana.port}";
            proxyWebsockets = true;
            extraConfig =
              # required when the target is also TLS server with multiple hosts
              "proxy_ssl_server_name on;" +
              # required when the server wants to use HTTP Authentication
              "proxy_pass_header Authorization;"
            ;
          };
          # locations."/chronograf" = {
          #   proxyPass = "http://127.0.0.1:${toString 8888}/";
          #   #proxyWebsockets = true;
          #   extraConfig =
          #     # required when the target is also TLS server with multiple hosts
          #     "proxy_ssl_server_name on;" +
          #     # required when the server wants to use HTTP Authentication
          #     #"proxy_pass_header Authorization;" +
          #     #"proxy_set_header Host $host;" +
          #     "proxy_ignore_client_abort on;"
          #   ;
          # };
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
