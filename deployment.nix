let
  region = "ap-southeast-1";
  app = (import ./.) {};
  accessKeyId = "default";
  ui = (import ./nix/snowman.nix).build { isJS = true; };
in
{
  network.description = "Chopaan and DB.";

  
  machine = { config, pkgs, resources, lib, ... }:
    let
      uijs = "${ui}/bin/ui.jsexe";
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

          instanceType = "t3.nano";

          ebsBoot = true;
          ebsInitialRootDiskSize = 100;

          keyPair = resources.ec2KeyPairs.chopaan-key-pair;

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
      };

      boot.loader.grub.device = lib.mkForce "/dev/nvme0n1";

      networking.firewall.allowedTCPPorts = [ 80 443 ];

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

        after = [ "docker-janusgraph.service" ];

        script =
          let
            chopaan = app.chopaan.kbtzim;
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
            server = app.chopaan.server;
          in
            ''
            ${server}/bin/server --assets ${uijs} --port ${toString serverPort} --tinkerHost ${tinkerHost} --tinkerPort ${toString janusPort}
            '';
      };

      
      users.users.nginx.extraGroups = [ "acme" ];
      security.acme.acceptTerms = true;
      security.acme.email = "faez@ecoenergy.global";
      security.acme.server = "https://acme-staging-v02.api.letsencrypt.org/directory";
      services.nginx = {
        enable = true;
        logError = "/dev/stdout info";
        recommendedTlsSettings = true;
        recommendedOptimisation = true;
        recommendedGzipSettings = true;
        recommendedProxySettings = true;
        
        virtualHosts.${dnsName} = {
          addSSL = true;
          enableACME = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:${toString serverPort}";
            root = uijs;
          };
        };
        virtualHosts."acmechallenge.${dnsName}" = {
          # Catchall vhost, will redirect users to HTTPS for all vhosts
          serverAliases = [ "*.${dnsName}" ];
          locations."/.well-known/acme-challenge" = {
            root = "/var/lib/acme/.challenges";
          };
          locations."/" = {
            return = "301 https://$host$request_uri";
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
    };
  }
