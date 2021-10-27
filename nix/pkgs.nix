pkgs: _: with pkgs; {
  chopaanHaskellPackages = import ./haskell.nix {
    inherit
      config
      lib
      stdenv
      haskell-nix
      pkgs
      buildPackages;
      #makeWrapper
      #jormungandr
      #cowsay;
  };
}
