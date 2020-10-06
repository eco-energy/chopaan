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
    flags = {};
    package = {
      specVersion = "1.18";
      identifier = { name = "concat-known"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2018 by Conal Elliott";
      maintainer = "conal@conal.net";
      author = "Conal Elliott";
      homepage = "";
      url = "";
      synopsis = "Entailments for KnownNat";
      description = "To eliminate when plugins no longer cause spurious recompilation.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
          (hsPkgs."ghc-typelits-knownnat" or (errorHandler.buildDepError "ghc-typelits-knownnat"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/conal/concat.git";
      rev = "6da06f44947ea8909fa3d60e94a238f7bc998bb6";
      sha256 = "1a3z21n0ispcc4irggkwsclfbv87z1gb52lfafwx09n8bszr8c19";
      });
    postUnpack = "sourceRoot+=/known; echo source root reset to \$sourceRoot";
    }