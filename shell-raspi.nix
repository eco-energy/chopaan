# shell.nix
{
# import nixpkgs with overlays
pkgs ? import ./nix { }

, withHoogle ? false
}:

with pkgs;
chopaanHaskellPackages.projectCross.raspberryPi.shellFor {
    # Include only the *local* packages of your project.
    packages = ps: with ps; [
      chopaan
    ];

    # Builds a Hoogle documentation index of all dependencies,
    # and provides a "hoogle" command to search the index.
    withHoogle = false;

    # Some common tools can be added with the `tools` argument
    tools = { cabal = "3.2.0.0"; hlint = "2.2.11"; };
    # See overlays/tools.nix for more details

    # Some you may need to get some other way.
    buildInputs = with pkgs;
      [ pkgs.git ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
}
