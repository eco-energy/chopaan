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
      specVersion = "2.2";
      identifier = { name = "Shpadoinkle-html"; version = "0.3.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "isaac.shapira@platonic.systems";
      author = "Isaac Shapira";
      homepage = "";
      url = "";
      synopsis = "A typed, template generated Html DSL, and helpers.";
      description = "Shpadoinkle Html is a typed template-generated Html DSL building on types provided\nby Shpadoinkle Core. This exports a large namespace of terms covering most of the\nHtml specifications. Some Elm-API-style helpers are present, but as outlaw type classes.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."Shpadoinkle" or (errorHandler.buildDepError "Shpadoinkle"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."compactable" or (errorHandler.buildDepError "compactable"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."ghcjs-dom" or (errorHandler.buildDepError "ghcjs-dom"))
          (hsPkgs."jsaddle" or (errorHandler.buildDepError "jsaddle"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."raw-strings-qq" or (errorHandler.buildDepError "raw-strings-qq"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          ] ++ (if compiler.isGhcjs && true
          then [
            (hsPkgs."ghcjs-base" or (errorHandler.buildDepError "ghcjs-base"))
            ]
          else [
            (hsPkgs."regex-pcre" or (errorHandler.buildDepError "regex-pcre"))
            ]);
        buildable = true;
        modules = [
          "Shpadoinkle/Html/Event/Basic"
          "Shpadoinkle/Html/Event/Debounce"
          "Shpadoinkle/Html/Event/Throttle"
          "Shpadoinkle/Html/TH/CSSTest"
          "Shpadoinkle/Html"
          "Shpadoinkle/Html/Element"
          "Shpadoinkle/Html/Event"
          "Shpadoinkle/Html/Property"
          "Shpadoinkle/Html/Memo"
          "Shpadoinkle/Html/MicroData"
          "Shpadoinkle/Html/Utils"
          "Shpadoinkle/Html/LocalStorage"
          "Shpadoinkle/Html/TH"
          "Shpadoinkle/Html/TH/CSS"
          "Shpadoinkle/WebWorker"
          "Shpadoinkle/Keyboard"
          ];
        hsSourceDirs = [ "./." ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././.source-repository-packages/11; }