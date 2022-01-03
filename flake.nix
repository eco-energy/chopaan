{ description = "Chopaan";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let

      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          chopaanProject =
            final.haskell-nix.project' {
              src = ./.;
              name = "chopaan";
              #stack-sha256 = "07xcy5j2qir1pnp2g2bznd21z1dfcmv860rzd7nd0iy061iwdi15";
              #materialized = ./chopaan.materialized;
              #checkMaterialization = true;
              compiler-nix-name = "ghc8107";
              modules = [
                {
                  packages.concat-inline.doHaddock = false;
                  packages.concat-plugin.doHaddock = false;
                  packages.concat-examples.doHaddock = true;

                }
              ];
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal =
                  { version = "3.2.0.0";
                    index-state = "2021-12-02T00:00:00Z";
                    plan-sha256 = "1l3561ifzhz25i2izivv89lqb1ya1rl2qnxa5hgy75xqaj8bxdaq";
                    materialized = ./nix/materialized/cabal;
                    checkMaterialization = true;
                  };
                #hlint = {};
                haskell-language-server =
                  { version = "latest";
                    index-state = "2021-12-02T00:00:00Z";
                    plan-sha256 = "1gjx7xi508yn2lrwl7ic1pnyhxzl38ylzy5v9pi9v2q8a6vxi3dd";
                    materialized = ./nix/materialized/hls;
                    checkMaterialization = true;
                  };
              };
              # Non-Haskell shell tools go here
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
              ];
              # This adds `js-unknown-ghcjs-cabal` to the shell.
              #shell.crossPlatform = p: [p.ghcjs];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.chopaanProject.flake {
        # This adds support for `nix build .#js-unknown-ghcjs-cabal:chopaan:exe:chopaan`
        crossPlatforms = p: [p.ghcjs];
      };
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."chopaan:lib:chopaan";
    });
}
