# shell.nix
{
# import nixpkgs with overlays
pkgs ? import ./nix {}

, withHoogle ? false
}:

with pkgs;
chopaanHaskellPackages.shellFor {
    # Include only the *local* packages of your project.
    packages = ps: with ps; [
      chopaan
    ];

    # Builds a Hoogle documentation index of all dependencies,
    # and provides a "hoogle" command to search the index.
    withHoogle = false;

    # You might want some extra tools in the shell (optional).

    # Some common tools can be added with the `tools` argument
    tools = {
      cabal =
        {  version = "3.2.0.0";
           index-state = "2021-12-02T00:00:00Z";
           plan-sha256 = "1l3561ifzhz25i2izivv89lqb1ya1rl2qnxa5hgy75xqaj8bxdaq";
           materialized = ./nix/materialized/cabal;
           checkMaterialization = false;
        };
      haskell-language-server = {
        version = "latest";
        index-state = "2021-12-02T00:00:00Z";
        plan-sha256 = "1gjx7xi508yn2lrwl7ic1pnyhxzl38ylzy5v9pi9v2q8a6vxi3dd";
        materialized = ./nix/materialized/hls;
        checkMaterialization = false;
      };
    };
    # See overlays/tools.nix for more details

    # Some you may need to get some other way.
    buildInputs = with pkgs;
      [ haskellPackages.ghcid
        pkgs.protobuf
        pkgs.postgresql
        pkgs.bazel
        pkgs.python3
      ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
}
