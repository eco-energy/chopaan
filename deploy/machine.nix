{ config, pkgs, resources, lib, hostName, grubDevice, ... }:
let
  #uijs = "${staticUi}/bin/ui.jsexe";
  region = "ap-southeast-1";
  app = (import ../. {}).chopaan;
  janusPort = 8182;
  serverPort = 8080;
  mqttPort = 8883;
  awskey = "/run/keys/aws-creds";
  tinkerHost = "localhost";
  janusConf = ../janusgraph-config;
  frontend = (import ../nix/website.nix) {};
  dashes = (import ../nix/dashboard.nix) {};
  withJanus = p: "${p} --tinkerHost ${tinkerHost} --tinkerPort ${toString janusPort}";
  withRTSOpts = p: "${p} +RTS -A32m -n4m -N";
  chopaanDir = "${config.users.users.chopaan.home}";
  dashboardDir = "/dash";
  isHttps = if (hostName == "localhost") then false else true;
in
{
  environment.systemPackages = [ pkgs.z3 ];
  nix.trustedUsers = lib.mkForce ["root" ];
  users = {
    users = {
      chopaan = {
        createHome = true;
        group = "chopaan";
        extraGroups = ["keys" "dash"];
        isSystemUser = true;
        home = "/chopaan";
        useDefaultShell = true;
      };
    };
    groups.chopaan = {};
    groups.dash = {};
  };

  deployment.keys = {
    aws-creds = {
      text = builtins.readFile ../credentials/key;
      user = "chopaan";
      group = "chopaan";
      permissions = "0640";
    };
  };
  
  boot.loader.grub.device = lib.mkForce grubDevice;
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
    after = [ "aws-creds-key.service" "network.target" "docker-janusgraph.service" ];        
    wantedBy = [ "multi-user.target" ];
    environment = {
      AWS_CREDS = awskey;
      XDG_ROOT_DIR = chopaanDir;
    };
    path = [ pkgs.z3 ];
    serviceConfig = {
      WorkingDirectory = "~";
      User = "chopaan";
      LimitNOFILE = 6400000;
    };
    script = withRTSOpts ((withJanus "${app.kbtzim}/bin/kbtzim"));
  };

  systemd.services.hydrate = {
    wantedBy = [ "multi-user.target" ];
    after = [ "aws-creds-key.service" "network.target" "docker-janusgraph.service" "chopaan.service" "influxdb.service" ];
    environment = {
      AWS_CREDS = awskey;
      XDG_ROOT_DIR = chopaanDir;
      STORE_PATH = "${chopaanDir}/data/hydration";
      S3_BUCKET = "dosti-datastream";
      START_DATE = "01-10-2021";
      PAST_RES = "Day";
      FUTURE_RES = "Minute";
      LIFETIME = "Infinite";
      MAN_CONN_COUNT = "100";
      MAN_IDLE_CONN = "0";
      MAN_TIMEOUT = "90";
      DL_THREADS = "300";
      SOURCE_GEN_THREADS = "1";
    };
    serviceConfig = {
      WorkingDirectory = "~";
      User = "chopaan";
      LimitNOFILE = 6400000;
    };
    script = withRTSOpts ((withJanus "${app.hydrate}/bin/hydrate"));
  };

  systemd.tmpfiles.rules = [
    "d ${dashboardDir} 0775 chopaan dash"
  ];
  systemd.services.dashgen = {
    wantedBy = [ "grafana.service" ];
    after = [ "chopaan.service" ];
    serviceConfig = {
        User = "chopaan";
        Group = "dash";
      };
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
    
    users.users.grafana.extraGroups = ["dash"];
    services.grafana = {
      enable = true;
      domain = hostName;
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
          { name = "Chopaan Flat";
          orgId = 1;
          type = "file";
          folder = "Chopaan_Flat";
          disableDeletion = false;
          updateIntervalSeconds = 30;
          options.path = "${dashes}";
          }
        ];
        datasources = [
          { name = "InfluxDB";
          type = "influxdb";
          access = "proxy";
          orgId = 1;
          url = "http://localhost:8086";
          editable = true;
          database = "chopaanS3";
          }
          { name = "InfluxDBMQTT";
          type = "influxdb";
          access = "proxy";
          orgId = 1;
          url = "http://localhost:8086";
          editable = true;
          database = "chopaanMQTT";
          }
        ];  
      };
    };
    
    virtualisation.virtualbox.guest.enable = lib.mkForce false;
    users.users.nginx.extraGroups = [ "acme" ];
    security.acme.acceptTerms = true;
    security.acme.email = "faez@ecoenergy.global";
    security.acme.server = lib.mkIf (!isHttps) "https://acme-staging-v02.api.letsencrypt.org/directory";
    services.nginx = {
      enable = true;
      logError = "stdout info";
      recommendedTlsSettings = isHttps;
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

      virtualHosts.${hostName} = {
        forceSSL = isHttps;
        enableACME = isHttps; 
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

      # virtualHosts.${hostName} = {
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
}
