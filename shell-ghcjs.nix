# shell.nix
{
# import nixpkgs with overlays
pkgs ? import ./nix { }

, withHoogle ? false
}:

with pkgs;
chopaanHaskellPackages.projectCross.ghcjs.shellFor {
    # Include only the *local* packages of your project.
    packages = ps: with ps; [
      chopaan
    ];

    # Builds a Hoogle documentation index of all dependencies,
    # and provides a "hoogle" command to search the index.
    withHoogle = false;


    # Some you may need to get some other way.
    buildInputs = with pkgs;
      [ pkgs.git ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = false;
}
