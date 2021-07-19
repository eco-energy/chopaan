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
      identifier = { name = "concat-plugin"; version = "0.3.0.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2016-2017 by Conal Elliott";
      maintainer = "conal@conal.net";
      author = "Conal Elliott";
      homepage = "";
      url = "";
      synopsis = "GHC plugin for compiling to categories";
      description = "GHC plugin for compiling to categories";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
          (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"))
          (hsPkgs."pointed" or (errorHandler.buildDepError "pointed"))
          (hsPkgs."keys" or (errorHandler.buildDepError "keys"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          (hsPkgs."finite-typelits" or (errorHandler.buildDepError "finite-typelits"))
          (hsPkgs."vector-sized" or (errorHandler.buildDepError "vector-sized"))
          (hsPkgs."concat-classes" or (errorHandler.buildDepError "concat-classes"))
          (hsPkgs."concat-satisfy" or (errorHandler.buildDepError "concat-satisfy"))
          ];
        buildable = true;
        modules = [
          "ConCat"
          "ConCat/Translators"
          "ConCat/OkType"
          "ConCat/Plugin"
          "ConCat/Rebox"
          ];
        hsSourceDirs = [ "src" ];
        includeDirs = [ "src" ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././.source-repository-packages/4; }