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
           index-state = "2021-08-13T00:00:00Z";
           #plan-sha256 = "1hxjlk1fmx9q89wxrbbfsz4g56ldaak2vf52blnv2vrk18ycla32";
           #materialized = ./nix/chopaan.materialized/cabal;
        };
    };
    # See overlays/tools.nix for more details

    # Some you may need to get some other way.
    buildInputs = with pkgs;
      [ haskellPackages.ghcid pkgs.protobuf pkgs.postgresql pkgs.bazel pkgs.python3 ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
}
