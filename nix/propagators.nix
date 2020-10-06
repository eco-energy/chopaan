{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { test-doctests = true; test-hlint = true; };
    package = {
      specVersion = "1.22";
      identifier = { name = "propagators"; version = "0"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2015 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/propagators/";
      url = "";
      synopsis = "The Art of the Propagator";
      description = "<http://web.mit.edu/~axch/www/art.pdf The Art of the Propagator>";
      buildType = "Custom";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."data-reify" or (errorHandler.buildDepError "data-reify"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."intervals" or (errorHandler.buildDepError "intervals"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."unique" or (errorHandler.buildDepError "unique"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/ekmett/propagators.git";
      rev = "6de416c68d7970d734dc139079337a20b078e45c";
      sha256 = "1l2dabwjybf33rrydi5a049iyrniczpbhg6387imghmfj0ibkijr";
      });
    }