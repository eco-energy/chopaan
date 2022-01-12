{ description = "Chopaan";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-2111";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixops-plugged.url = "github:lukebfox/nixops-plugged";
  inputs.sops-nix.url = "github:Mic92/sops-nix";
  inputs.sops-nix.inputs.nixpkgs.follows = "haskellNix/nixpkgs-2111";
  outputs = { self, nixpkgs, flake-utils, haskellNix, nixops-plugged, sops-nix }:
    let
     linux = "x86_64-linux";
     f = flake-utils.lib.eachSystem [ linux ] (system:
      let
        projectName = "chopaan";
        branchMap = {
          "https://github.com/faezs/net-spider.git" = "bidirectional-neighborhood";
          "https://github.com/brendanhay/amazonka.git" = "main";
        };
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our proect to pkgs
          chopaan =
            final.haskell-nix.project' {
              src = final.haskell-nix.haskellLib.cleanGit {
                src = ./.;
                name = "${projectName}-src";
                keepGitDir = true;
              };
              name = projectName;
              compiler-nix-name = "ghc8107";
              stack-sha256 = "1znwg9jxi6mbsdj4ih6wb4gvcm9cyrav8qjmmaljypyz8lw32ll5";
              materialized = ./nix/materialized/flake/chopaan;
              #checkMaterialization = true;
              modules = [
                  { doHaddock = true;
                    doCheck = false;
                    packages.${projectName} = {
                      package.cleanHpack = true;
                    };
                    packages.concat-inline.doHaddock = false;
                  }
              ];
              branchMap = branchMap;
              lookupBranch = { location, ... }: (branchMap."${location}" or null);
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hoogle` and `haskell-language-server`
              shell.tools = tools;
              # Non-Haskell shell tools go here
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
                nixops-plugged.defaultPackage.${system}
              ];
              #shell.extraShells
              # This adds `js-unknown-ghcjs-cabal` to the shell.
              # shell.crossPlatform = p: [p.ghcjs];
            };
        })
                 ];
      tools = {
        cabal =
          { version = "latest";
            index-state = "2021-12-02T00:00:00Z";
            plan-sha256 = "03i9rdvnpkr96x3ng5zfvfd9h49qsyzmxlckh2i1yr4xn991yid3";
            materialized = ./nix/materialized/flake/cabal;
          };
        haskell-language-server =
          { version = "latest";
            index-state = "2021-12-02T00:00:00Z";
            plan-sha256 = "1gjx7xi508yn2lrwl7ic1pnyhxzl38ylzy5v9pi9v2q8a6vxi3dd";
            materialized = ./nix/materialized/flake/haskell-language-server;
          };
        hoogle = 
          { version = "latest";
            index-state = "2021-12-02T00:00:00Z";
            plan-sha256 = "0j7y117792f2sbwcxc1d7jlgx7kbcgp0v11ryxwldadbqmn6b66b";
            materialized = ./nix/materialized/flake/hoogle;
          };
      };
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      project = pkgs.${projectName};
      devShell = project.shellFor {
        packages = ps: [ ps.${projectName} ];
        exactDeps = true;
        tools = tools;
      };
      flake = project.flake {
        # This adds support for `nix build .#js-unknown-ghcjs-cabal:${projectName}:exe:${projectName}`
        # crossPlatforms = p: [p.ghcjs];
      };
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."${projectName}:exe:kbtzim";
      nixopsConfigurations.default = ({
        inherit nixpkgs;
        network.storage.legacy = {};
        network.description = "${projectName} - Flake";
        defaults = { ... }: {
          imports = [{
            imports = [ ./deploy/secrets.nix sops-nix.nixosModules.sops ];
          }];
          nixpkgs.pkgs = pkgs;
          _module.args = { app = flake.packages."${projectName}:exe:kbtzim";
                           inherit sops-nix;
                         };
        };
      } // (import ./deployment.nix));
      
      # Adds a link farm which collects all the SHAs and materializers force
      # the project and acts as a gcroot for the dependencies so they're not
      # removed when nix-store --gc is run
      packages = flake.packages // {
        gcroot = pkgs.linkFarmFromDrvs "${projectName}-shell-gcroot" [
            devShell
            devShell.stdenv
            devShell.buildInputs
            pkgs.${projectName}.stack-nix
            pkgs.${projectName}.roots
            (
              let compose = f: g: x: f (g x);
                  flakePaths = compose pkgs.lib.attrValues (
                    pkgs.lib.mapAttrs
                      (name: flake: { name = name; path = flake.outPath; })
                  );
              in  pkgs.linkFarm "input-flakes" (flakePaths self.inputs)
            )

            (
              let passthru = p: if
                    builtins.hasAttr "stack-nix" p
                    then p.stack-nix.passthru
                    else p.plan-nix.passthru;
                  getMaterializers = ( name: project:
                    pkgs.linkFarmFromDrvs "${name}" [
                      (passthru project).calculateMaterializedSha
                      (passthru project).generateMaterialized
                    ]
                  );
              in
                pkgs.linkFarmFromDrvs "materializers" (
                  pkgs.lib.mapAttrsToList getMaterializers (
                      { ${projectName} = project; }
                      // (pkgs.lib.mapAttrs (_: builtins.getAttr "project") (project.tools tools))
                  )
                )
            )
          ];
      };
    });
    in f // { nixopsConfigurations.default = f.nixopsConfigurations.${linux}.default; }; 
}
