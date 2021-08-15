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
      identifier = { name = "concat-inline"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2016-2017 by Conal Elliott";
      maintainer = "conal@conal.net";
      author = "Conal Elliott";
      homepage = "";
      url = "";
      synopsis = "Simple GHC plugin for inlining class-ops";
      description = "Simple GHC plugin for inlining class-ops";
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
          ];
        buildable = true;
        modules = [
          "ConCat/Inline/ClassOp"
          "ConCat/Inline/Plugin"
          "ConCat/Inline/SampleMethods"
          ];
        hsSourceDirs = [ "src" ];
        includeDirs = [ "src" ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././.source-repository-packages/0; }