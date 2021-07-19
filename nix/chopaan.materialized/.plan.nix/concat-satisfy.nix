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
      identifier = { name = "concat-satisfy"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2018 by Conal Elliott";
      maintainer = "conal@conal.net";
      author = "Conal Elliott";
      homepage = "";
      url = "";
      synopsis = "GHC plugin for forcing constraint satisfaction";
      description = "GHC plugin for forcing constraint satisfaction";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
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
          (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          (hsPkgs."concat-inline" or (errorHandler.buildDepError "concat-inline"))
          ];
        buildable = true;
        modules = [
          "ConCat/Simplify"
          "ConCat/BuildDictionary"
          "ConCat/Satisfy"
          "ConCat/Satisfy/Plugin"
          ];
        hsSourceDirs = [ "src" ];
        includeDirs = [ "src" ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././.source-repository-packages/2; }