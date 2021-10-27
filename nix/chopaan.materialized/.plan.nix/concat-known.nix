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
          (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
          (hsPkgs."ghc-typelits-knownnat" or (errorHandler.buildDepError "ghc-typelits-knownnat"))
          ];
        buildable = true;
        modules = [ "ConCat/Known" ];
        hsSourceDirs = [ "src" ];
        includeDirs = [ "src" ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././.source-repository-packages/1; }