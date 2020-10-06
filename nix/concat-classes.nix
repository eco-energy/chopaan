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
      identifier = { name = "concat-classes"; version = "0.3.0.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2016-2017 by Conal Elliott";
      maintainer = "conal@conal.net";
      author = "Conal Elliott";
      homepage = "";
      url = "";
      synopsis = "Constrained categories";
      description = "Constrained categories for compiling to categories";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."finite-typelits" or (errorHandler.buildDepError "finite-typelits"))
          (hsPkgs."vector-sized" or (errorHandler.buildDepError "vector-sized"))
          (hsPkgs."free" or (errorHandler.buildDepError "free"))
          (hsPkgs."keys" or (errorHandler.buildDepError "keys"))
          (hsPkgs."pointed" or (errorHandler.buildDepError "pointed"))
          (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
          (hsPkgs."adjunctions" or (errorHandler.buildDepError "adjunctions"))
          (hsPkgs."prettyclass" or (errorHandler.buildDepError "prettyclass"))
          (hsPkgs."concat-inline" or (errorHandler.buildDepError "concat-inline"))
          (hsPkgs."concat-satisfy" or (errorHandler.buildDepError "concat-satisfy"))
          (hsPkgs."concat-known" or (errorHandler.buildDepError "concat-known"))
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
    postUnpack = "sourceRoot+=/classes; echo source root reset to \$sourceRoot";
    }