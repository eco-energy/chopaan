{
  description = "Chopaan solver benchmark";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          criterion
          sbv
          containers
          deepseq
        ]);
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [ ghc pkgs.z3 ];
        };

        packages.solver-bench = pkgs.stdenv.mkDerivation {
          name = "solver-bench";
          src = ./benchmark;
          buildInputs = [ ghc pkgs.z3 ];
          buildPhase = ''
            ghc -O2 -threaded -o solver-bench SolverBench.hs
          '';
          installPhase = ''
            mkdir -p $out/bin
            cp solver-bench $out/bin/
          '';
        };

        apps.bench = {
          type = "app";
          program = "${self.packages.${system}.solver-bench}/bin/solver-bench";
        };
      });
}
