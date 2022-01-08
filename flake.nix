{ description = "Chopaan";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-2111";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixops-plugged.url = "github:lukebfox/nixops-plugged";
  outputs = { self, nixpkgs, flake-utils, haskellNix, nixops-plugged }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        projectName = "chopaan";
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          chopaan =
            final.haskell-nix.project' {
              src = ./.;
                # let
                #   cleanGitHaskell = {src, name } :
                #     let
                #       clean = final.haskell-nix.haskellLib.cleanGit {
                #         name = "${name}-gitClean"; inherit src;
                #       };
                #     in
                #       final.haskell-nix.cleanSourceHaskell {
                #         inherit name;
                #         src = clean;
                #       };
                # in cleanGitHaskell { name=projectName; src = ./.; };

              name = projectName;
              #stack-sha256 = "07xcy5j2qir1pnp2g2bznd21z1dfcmv860rzd7nd0iy061iwdi15";
              #materialized = ./nix/materialized/flake;
              #checkMaterialization = false;
              compiler-nix-name = "ghc8107";
              modules = [
                { doHaddock = false;
                  packages.${projectName}.doHaddock = false;
                  packages.concat-inline.doHaddock = false;
                  packages.concat-plugin.doHaddock = false;
                  packages.concat-examples.doHaddock = false;

                }
              ];
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
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
          { version = "latest"; };
        haskell-language-server =
          { version = "latest";
            index-state = "2021-12-02T00:00:00Z";
            plan-sha256 = "1gjx7xi508yn2lrwl7ic1pnyhxzl38ylzy5v9pi9v2q8a6vxi3dd";
            materialized = ./nix/materialized/hls;
          };
      };
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      project = pkgs.${projectName};
      devShell = project.shellFor {
        packages = ps: [ ps.${projectName} ];
        exactDeps = true;
        tools = tools;
      };
      flake = pkgs.${projectName}.flake {
        # This adds support for `nix build .#js-unknown-ghcjs-cabal:${projectName}:exe:${projectName}`
        # crossPlatforms = p: [p.ghcjs];
      };
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."${projectName}:exe:kbtzim";
      #app = pkgs.${projectName}.stack-nix.passthru;
      packages = flake.packages // {
        gcroot = pkgs.linkFarmFromDrvs "${projectName}-shell-gcroot" [
            devShell
            devShell.stdenv
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
              let passthru = if
                    builtins.hasAttr "stack-nix" project
                    then project.stack-nix.passthru
                    else project.plan-nix.passthru;
                  getMaterializers = ( name: project:
                    pkgs.linkFarmFromDrvs "${name}" [
                      passthru.calculateMaterializedSha
                      passthru.generateMaterialized
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
}
