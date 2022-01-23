{ config, pkgs, resources, lib, hostName, grubDevice, chopaan, sops-nix, ... }:
let
  region = "ap-southeast-1";
  janusPort = 8182;
  serverPort = 8080;
  mqttPort = 8883;
  awskey = "/run/keys/aws-creds";
  tinkerHost = "localhost";
  withRTSOpts = p: "${p} +RTS -A32m -n4m -N";
  chopaanDir = "${config.users.users.chopaan.home}";
  dashboardDir = "/dash";
  isHttps = if (hostName == "localhost") then false else true; 
in
{
  imports = [
    (import ./secrets.nix { inherit config pkgs lib sops-nix; })
  ];  
    
  environment.systemPackages = [ pkgs.z3 ];
  nix.trustedUsers = lib.mkForce ["root" ];
  users = {
    users = {
      chopaan = {
        createHome = true;
        group = "chopaan";
        extraGroups = ["keys" "dash" config.users.groups.keys.name ];
        isSystemUser = true;
        home = "/chopaanFS/chopaan/";
        useDefaultShell = true;
      };
    };
    groups.chopaan = {};
    groups.dash = {};
  };

  #boot.loader.grub.device = lib.mkForce grubDevice;
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
  
  systemd.extraConfig = "DefaultLimitNOFILE=6400000\nDefaultStandardError='journal'\nDefaultStandardOut='journal'";

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_13;
    extraPlugins = [ pkgs.timescaledb ];
    settings = {
      shared_preload_libraries = "timescaledb";
    };
  };
  
  systemd.services.chopaan = {
    after = [ #"aws-creds-key.service"
              "chopaanFS.mount"
              "network.target"
              "docker-janusgraph.service"
              "influxdb.service"
            ];        
    wantedBy = [ "multi-user.target" ];
    environment = {
      AWS_CREDS = config.sops.secrets.aws-creds.path;
      XDG_ROOT_DIR = chopaanDir;
      STORE_PATH = "./data/hydration";
      S3_BUCKET = "dosti-datastream";
      START_DATE = "14-10-2021";
      PAST_RES = "Ten5";
      FUTURE_RES = "Ten1";
      LIFETIME = "Infinite";
      MAN_CONN_COUNT = "128";
      MAN_IDLE_CONN = "64";
      MAN_TIMEOUT = "90";
      DL_THREADS = "30";
      SOURCE_GEN_THREADS = "12";
    };
    path = [ pkgs.z3 ];
    serviceConfig = {
      WorkingDirectory = "~";
      User = "chopaan";
      Group = "chopaan";
      LimitNOFILE = 6400000;
    };
    script = withRTSOpts "${chopaan.kbtzim}/bin/kbtzim";
  };

  systemd.tmpfiles.rules = [
    "d ${dashboardDir} 0775 chopaan dash"
    "d /chopaanFS 0755 root root"
    "d /chopaanFS/chopaan 0755 chopaan chopaan"
    "d /chopaanFS/influx 0775 influxdb influxdb"
    "d /chopaanFS/grafana 0775 grafana grafana"
  ];
  # systemd.services.dashgen = {
  #   wantedBy = [ "grafana.service" ];
  #   after = [ "chopaan.service" ];
  #   serviceConfig = {
  #       User = "chopaan";
  #       Group = "dash";
  #     };
  #   unitConfig.RequiresMountsFor = dashboardDir;
  #   script = (withJanus "${app.dashgen}/bin/dashgen --outpath ${dashboardDir}");
  # };

  services.influxdb = {
    enable = true;
    dataDir = "/chopaanFS/influx";
    extraConfig = {
      collectd = [{ enabled = false; }];
      udp = [{ enabled = true; }];
    };
  };
    
  users.users.grafana.extraGroups = ["dash"];
  services.grafana = {
    enable = true;
    domain = hostName;
    port = 2342;
    addr = "127.0.0.1";
    dataDir = "/chopaanFS/grafana";
    security = {
      adminPasswordFile = config.sops.secrets.grafanaAdmin.path;
    };
    provision = {
      enable = true;
      dashboards = [
        { name = "Chopaan Dash";
          orgId = 1;
          type = "file";
          folder = "Chopaan";
          disableDeletion = false;
          updateIntervalSeconds = 60;
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
    
  virtualisation.virtualbox.guest.enable = lib.mkIf (!isHttps) (lib.mkForce false);
  users.users.nginx.extraGroups = [ "acme" ];
  security.acme.acceptTerms = true;
  security.acme.email = "faez@ecoenergy.global";
  security.acme.server = lib.mkIf (!isHttps) "https://acme-staging-v02.api.letsencrypt.org/directory";
  services.nginx = {
    enable = true;
    logError = "stderr info";
    recommendedTlsSettings = isHttps;
    recommendedOptimisation = true;
    recommendedGzipSettings = true;
    recommendedProxySettings = true;
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
    };
  };
}
