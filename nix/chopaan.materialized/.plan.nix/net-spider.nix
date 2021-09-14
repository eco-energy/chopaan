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
    flags = { server-test = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "net-spider"; version = "0.4.3.6"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Toshio Ito <debug.ito@gmail.com>";
      author = "Toshio Ito <debug.ito@gmail.com>";
      homepage = "https://github.com/debug-ito/net-spider";
      url = "";
      synopsis = "A graph database middleware to maintain a time-varying graph.";
      description = "A graph database middleware to maintain a time-varying graph. See the [project README](https://github.com/debug-ito/net-spider) for detail.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" "ChangeLog.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."greskell-websocket" or (errorHandler.buildDepError "greskell-websocket"))
          (hsPkgs."greskell" or (errorHandler.buildDepError "greskell"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-interval" or (errorHandler.buildDepError "data-interval"))
          (hsPkgs."extended-reals" or (errorHandler.buildDepError "extended-reals"))
          (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."regex-applicative" or (errorHandler.buildDepError "regex-applicative"))
          ];
        buildable = true;
        modules = [
          "NetSpider/Graph/Internal"
          "NetSpider/Spider/Internal/Graph"
          "NetSpider/Spider/Internal/Log"
          "NetSpider/Spider/Internal/Spider"
          "NetSpider/Queue"
          "NetSpider/Query/Internal"
          "NetSpider"
          "NetSpider/Input"
          "NetSpider/Output"
          "NetSpider/Found"
          "NetSpider/Timestamp"
          "NetSpider/Spider"
          "NetSpider/Spider/Config"
          "NetSpider/Unify"
          "NetSpider/Snapshot"
          "NetSpider/Graph"
          "NetSpider/Pair"
          "NetSpider/Query"
          "NetSpider/Log"
          "NetSpider/Snapshot/Internal"
          "NetSpider/GraphML/Writer"
          "NetSpider/GraphML/Attribute"
          "NetSpider/Interval"
          "NetSpider/Weaver"
          "NetSpider/SeqID"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."net-spider" or (errorHandler.buildDepError "net-spider"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."greskell" or (errorHandler.buildDepError "greskell"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          modules = [
            "NetSpider/GraphML/WriterSpec"
            "NetSpider/TimestampSpec"
            "NetSpider/FoundSpec"
            "NetSpider/SnapshotSpec"
            "NetSpider/WeaverSpec"
            "NetSpider/SeqIDSpec"
            "JSONUtil"
            "SnapshotTestCase"
            "TestCommon"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        "server-test-suite" = {
          depends = (pkgs.lib).optionals (flags.server-test) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."net-spider" or (errorHandler.buildDepError "net-spider"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."greskell-websocket" or (errorHandler.buildDepError "greskell-websocket"))
            (hsPkgs."greskell" or (errorHandler.buildDepError "greskell"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
            (hsPkgs."hspec-need-env" or (errorHandler.buildDepError "hspec-need-env"))
            ];
          buildable = if flags.server-test then true else false;
          modules = [
            "TestCommon"
            "SnapshotTestCase"
            "ServerTest/Snapshot"
            "ServerTest/Attributes"
            "ServerTest/ServerCommon"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "ServerTest.hs" ];
          };
        "doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."doctest-discover" or (errorHandler.buildDepError "doctest-discover"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.doctest-discover.components.exes.doctest-discover or (pkgs.buildPackages.doctest-discover or (errorHandler.buildToolDepError "doctest-discover:doctest-discover")))
            ];
          buildable = true;
          hsSourceDirs = [ "test" ];
          mainPath = [ "DocTest.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././.source-repository-packages/22; }