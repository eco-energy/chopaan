{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = { prod = false; };
    package = {
      specVersion = "1.12";
      identifier = { name = "chopaan"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "2019 Faez Shakil";
      maintainer = "faez.shakil@gmail.com";
      author = "Faez Shakil";
      homepage = "https://github.com/faezs/chopaan#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on Github at <https://github.com/faezs/chopaan#readme>";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [ "options.dhall" ];
      extraSrcFiles = [
        "README.md"
        "ChangeLog.md"
        "assets/style.css"
        "assets/tailwind.min.css"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."Shpadoinkle" or (errorHandler.buildDepError "Shpadoinkle"))
          (hsPkgs."Shpadoinkle-backend-pardiff" or (errorHandler.buildDepError "Shpadoinkle-backend-pardiff"))
          (hsPkgs."Shpadoinkle-backend-snabbdom" or (errorHandler.buildDepError "Shpadoinkle-backend-snabbdom"))
          (hsPkgs."Shpadoinkle-console" or (errorHandler.buildDepError "Shpadoinkle-console"))
          (hsPkgs."Shpadoinkle-html" or (errorHandler.buildDepError "Shpadoinkle-html"))
          (hsPkgs."Shpadoinkle-lens" or (errorHandler.buildDepError "Shpadoinkle-lens"))
          (hsPkgs."Shpadoinkle-router" or (errorHandler.buildDepError "Shpadoinkle-router"))
          (hsPkgs."Shpadoinkle-template" or (errorHandler.buildDepError "Shpadoinkle-template"))
          (hsPkgs."Shpadoinkle-widgets" or (errorHandler.buildDepError "Shpadoinkle-widgets"))
          (hsPkgs."ad" or (errorHandler.buildDepError "ad"))
          (hsPkgs."adjunctions" or (errorHandler.buildDepError "adjunctions"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."algebraic-graphs" or (errorHandler.buildDepError "algebraic-graphs"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base64" or (errorHandler.buildDepError "base64"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
          (hsPkgs."clay" or (errorHandler.buildDepError "clay"))
          (hsPkgs."colour" or (errorHandler.buildDepError "colour"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          (hsPkgs."compensated" or (errorHandler.buildDepError "compensated"))
          (hsPkgs."constraints-extras" or (errorHandler.buildDepError "constraints-extras"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."convertible" or (errorHandler.buildDepError "convertible"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."dependent-map" or (errorHandler.buildDepError "dependent-map"))
          (hsPkgs."dependent-sum" or (errorHandler.buildDepError "dependent-sum"))
          (hsPkgs."dependent-sum-template" or (errorHandler.buildDepError "dependent-sum-template"))
          (hsPkgs."diagrams-contrib" or (errorHandler.buildDepError "diagrams-contrib"))
          (hsPkgs."diagrams-lib" or (errorHandler.buildDepError "diagrams-lib"))
          (hsPkgs."diagrams-svg" or (errorHandler.buildDepError "diagrams-svg"))
          (hsPkgs."dimensional" or (errorHandler.buildDepError "dimensional"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
          (hsPkgs."envy" or (errorHandler.buildDepError "envy"))
          (hsPkgs."estimator" or (errorHandler.buildDepError "estimator"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."generic-data" or (errorHandler.buildDepError "generic-data"))
          (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."ghcjs-dom" or (errorHandler.buildDepError "ghcjs-dom"))
          (hsPkgs."greskell" or (errorHandler.buildDepError "greskell"))
          (hsPkgs."greskell-core" or (errorHandler.buildDepError "greskell-core"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."haxl" or (errorHandler.buildDepError "haxl"))
          (hsPkgs."higgledy" or (errorHandler.buildDepError "higgledy"))
          (hsPkgs."interpolatedstring-perl6" or (errorHandler.buildDepError "interpolatedstring-perl6"))
          (hsPkgs."jsaddle" or (errorHandler.buildDepError "jsaddle"))
          (hsPkgs."keys" or (errorHandler.buildDepError "keys"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."linear" or (errorHandler.buildDepError "linear"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."monad-bayes" or (errorHandler.buildDepError "monad-bayes"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
          (hsPkgs."one-liner" or (errorHandler.buildDepError "one-liner"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."pointed" or (errorHandler.buildDepError "pointed"))
          (hsPkgs."proto-lens" or (errorHandler.buildDepError "proto-lens"))
          (hsPkgs."proto-lens-runtime" or (errorHandler.buildDepError "proto-lens-runtime"))
          (hsPkgs."raw-strings-qq" or (errorHandler.buildDepError "raw-strings-qq"))
          (hsPkgs."regex-applicative" or (errorHandler.buildDepError "regex-applicative"))
          (hsPkgs."resource-pool" or (errorHandler.buildDepError "resource-pool"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."retry" or (errorHandler.buildDepError "retry"))
          (hsPkgs."rio" or (errorHandler.buildDepError "rio"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."servant-client-js" or (errorHandler.buildDepError "servant-client-js"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
          (hsPkgs."streamly-bytestring" or (errorHandler.buildDepError "streamly-bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
          (hsPkgs."unagi-chan" or (errorHandler.buildDepError "unagi-chan"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."vector-sized" or (errorHandler.buildDepError "vector-sized"))
          ] ++ (if !(compiler.isGhcjs && true)
          then [
            (hsPkgs."amazonka" or (errorHandler.buildDepError "amazonka"))
            (hsPkgs."amazonka-iot" or (errorHandler.buildDepError "amazonka-iot"))
            (hsPkgs."amazonka-s3" or (errorHandler.buildDepError "amazonka-s3"))
            (hsPkgs."cas-hashable" or (errorHandler.buildDepError "cas-hashable"))
            (hsPkgs."cas-store" or (errorHandler.buildDepError "cas-store"))
            (hsPkgs."composite-ekg" or (errorHandler.buildDepError "composite-ekg"))
            (hsPkgs."concat-classes" or (errorHandler.buildDepError "concat-classes"))
            (hsPkgs."concat-examples" or (errorHandler.buildDepError "concat-examples"))
            (hsPkgs."concat-graphics" or (errorHandler.buildDepError "concat-graphics"))
            (hsPkgs."concat-plugin" or (errorHandler.buildDepError "concat-plugin"))
            (hsPkgs."concurrent-dns-cache" or (errorHandler.buildDepError "concurrent-dns-cache"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."connection" or (errorHandler.buildDepError "connection"))
            (hsPkgs."dhall" or (errorHandler.buildDepError "dhall"))
            (hsPkgs."directory-tree" or (errorHandler.buildDepError "directory-tree"))
            (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
            (hsPkgs."ekg" or (errorHandler.buildDepError "ekg"))
            (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
            (hsPkgs."fusion-plugin" or (errorHandler.buildDepError "fusion-plugin"))
            (hsPkgs."greskell-websocket" or (errorHandler.buildDepError "greskell-websocket"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."influxdb" or (errorHandler.buildDepError "influxdb"))
            (hsPkgs."katip" or (errorHandler.buildDepError "katip"))
            (hsPkgs."net-mqtt" or (errorHandler.buildDepError "net-mqtt"))
            (hsPkgs."net-spider" or (errorHandler.buildDepError "net-spider"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."sbv" or (errorHandler.buildDepError "sbv"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
            (hsPkgs."servant-js" or (errorHandler.buildDepError "servant-js"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."servant-websockets" or (errorHandler.buildDepError "servant-websockets"))
            (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."vinyl" or (errorHandler.buildDepError "vinyl"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."wai-cors" or (errorHandler.buildDepError "wai-cors"))
            (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
            (hsPkgs."winery" or (errorHandler.buildDepError "winery"))
            (hsPkgs."x509-store" or (errorHandler.buildDepError "x509-store"))
            (hsPkgs."x509-validation" or (errorHandler.buildDepError "x509-validation"))
            ]
          else [
            (hsPkgs."ghcjs-base" or (errorHandler.buildDepError "ghcjs-base"))
            ]);
        buildable = true;
        modules = [ "Paths_chopaan" ] ++ (if !(compiler.isGhcjs && true)
          then [
            "Chopaan"
            "Chopaan/Client"
            "Chopaan/API/History"
            "Chopaan/Graph"
            "Chopaan/Graph/Greskell"
            "Chopaan/Graph/G"
            "Chopaan/Graph/Snapshot"
            "Chopaan/CRUD"
            "Chopaan/Ui/Base"
            "Chopaan/Ui/Events"
            "Chopaan/Ui/AddKbtz"
            "Chopaan/Ui/Foreign/Utils"
            "Chopaan/Ui/Foreign/WebSocket"
            "Chopaan/Ui/FormCommon"
            "Chopaan/Ui/Grable"
            "Chopaan/Ui/GraphView"
            "Chopaan/Ui/ImageV"
            "Chopaan/Ui/Style"
            "Chopaan/Ui/WebGL"
            "Chopaan/Ui/WebSocket"
            "Chopaan/Ui/ThreeD"
            "Chopaan/Ui/Interaction"
            "Chopaan/Ui/Timeline"
            "Chopaan/Ui/Tables"
            "Chopaan/UiTypes"
            "Chopaan/View"
            "Chopaan/Monad/Env"
            "Chopaan/Node/Calibration"
            "Chopaan/Node/Components"
            "Chopaan/Node/Folds"
            "Chopaan/Node/HW"
            "Chopaan/Node/Mesh"
            "Chopaan/Node/Metrics"
            "Chopaan/Node/Node"
            "Chopaan/Node/NodeId"
            "Chopaan/Node/NodeOpts"
            "Chopaan/Node/NodeSensors"
            "Chopaan/Node/NodeT"
            "Chopaan/Node/SoC"
            "Chopaan/Node/Storage"
            "Chopaan/Node/Structure"
            "Chopaan/Node/Tf"
            "Chopaan/Comm/Address"
            "Chopaan/Comm/Dispatch"
            "Chopaan/Comm/Comm"
            "Chopaan/Comm/Mqtt"
            "Chopaan/Comm/Mqtt/AWS"
            "Chopaan/Comm/Queues"
            "Chopaan/Comm/S3"
            "Chopaan/Comm/Monitor"
            "Chopaan/Comm/Socket"
            "Chopaan/Graph/Kbtz"
            "Chopaan/Graph/Spider"
            "Chopaan/Graph/VI"
            "Chopaan/Haxl/Kbtz"
            "Chopaan/Kibbutz"
            "Chopaan/Hydrate"
            "Chopaan/Hydration/Store"
            "Chopaan/Kibbutz/KbtzId"
            "Chopaan/Kibbutz/Kibbutz"
            "Chopaan/Kibbutz/DataSource"
            "Chopaan/Kibbutz/Transactor"
            "Chopaan/Kibbutz/Allocate"
            "Chopaan/Kibbutz/AWS/Common"
            "Chopaan/Kibbutz/AWS/Things"
            "Chopaan/Kibbutz/LinOpt"
            "Chopaan/Kibbutz/Registry"
            "Chopaan/Kibbutz/Serve"
            "Chopaan/Run"
            "Chopaan/Server"
            "Chopaan/Testing"
            "Chopaan/Types"
            "Chopaan/Utils/JSON"
            "Chopaan/Utils/Retry"
            "Chopaan/Utils/Streamly"
            "Chopaan/Utils/StreamsInterop"
            "Chopaan/Utils/Time"
            "Data/Selectors"
            "Data/Influxable"
            "Data/HList"
            "Data/BTreeIndex"
            "Data/BTreeIndex/Types"
            "Kbtz"
            "Proto/NodeMessageSchema/NodeMessages"
            "Proto/NodeMessageSchema/NodeMessages_Fields"
            "Servant/Streamly"
            "SimNode"
            "Streamly/Binary"
            "TxModel"
            ]
          else [
            "Chopaan/Types"
            "Chopaan/Client"
            "Chopaan/API/History"
            "Chopaan/Graph/Greskell"
            "Chopaan/Graph/G"
            "Chopaan/Graph"
            "Chopaan/Graph/Snapshot"
            "Chopaan/CRUD"
            "Chopaan/Ui/Base"
            "Chopaan/Ui/Events"
            "Chopaan/Ui/AddKbtz"
            "Chopaan/Ui/Foreign/Utils"
            "Chopaan/Ui/Foreign/WebSocket"
            "Chopaan/Ui/FormCommon"
            "Chopaan/Ui/Grable"
            "Chopaan/Ui/GraphView"
            "Chopaan/Ui/Style"
            "Chopaan/Ui/WebGL"
            "Chopaan/Ui/WebSocket"
            "Chopaan/Ui/ThreeD"
            "Chopaan/Ui/Interaction"
            "Chopaan/Ui/Timeline"
            "Chopaan/UiTypes"
            "Chopaan/View"
            "Chopaan/Kibbutz/KbtzId"
            "Chopaan/Kibbutz/Transactor"
            "Chopaan/Monad/Env"
            "Chopaan/Node/Calibration"
            "Chopaan/Node/Components"
            "Chopaan/Node/Folds"
            "Chopaan/Node/HW"
            "Chopaan/Node/Mesh"
            "Chopaan/Node/Metrics"
            "Chopaan/Node/Node"
            "Chopaan/Node/NodeId"
            "Chopaan/Node/NodeSensors"
            "Chopaan/Node/NodeT"
            "Chopaan/Node/SoC"
            "Chopaan/Node/Storage"
            "Chopaan/Utils/JSON"
            "Chopaan/Utils/Retry"
            "Chopaan/Utils/Streamly"
            "Chopaan/Utils/Time"
            "Data/Selectors"
            "Servant/Streamly"
            "Proto/NodeMessageSchema/NodeMessages"
            "Proto/NodeMessageSchema/NodeMessages_Fields"
            ]);
        hsSourceDirs = [
          "src"
          ] ++ (pkgs.lib).optional (!(!(compiler.isGhcjs && true))) "src";
        };
      exes = {
        "dashgen" = {
          depends = ([
            (hsPkgs."Shpadoinkle" or (errorHandler.buildDepError "Shpadoinkle"))
            (hsPkgs."Shpadoinkle-backend-pardiff" or (errorHandler.buildDepError "Shpadoinkle-backend-pardiff"))
            (hsPkgs."Shpadoinkle-backend-snabbdom" or (errorHandler.buildDepError "Shpadoinkle-backend-snabbdom"))
            (hsPkgs."Shpadoinkle-console" or (errorHandler.buildDepError "Shpadoinkle-console"))
            (hsPkgs."Shpadoinkle-html" or (errorHandler.buildDepError "Shpadoinkle-html"))
            (hsPkgs."Shpadoinkle-lens" or (errorHandler.buildDepError "Shpadoinkle-lens"))
            (hsPkgs."Shpadoinkle-router" or (errorHandler.buildDepError "Shpadoinkle-router"))
            (hsPkgs."Shpadoinkle-template" or (errorHandler.buildDepError "Shpadoinkle-template"))
            (hsPkgs."Shpadoinkle-widgets" or (errorHandler.buildDepError "Shpadoinkle-widgets"))
            (hsPkgs."ad" or (errorHandler.buildDepError "ad"))
            (hsPkgs."adjunctions" or (errorHandler.buildDepError "adjunctions"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."algebraic-graphs" or (errorHandler.buildDepError "algebraic-graphs"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64" or (errorHandler.buildDepError "base64"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."clay" or (errorHandler.buildDepError "clay"))
            (hsPkgs."colour" or (errorHandler.buildDepError "colour"))
            (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
            (hsPkgs."compensated" or (errorHandler.buildDepError "compensated"))
            (hsPkgs."constraints-extras" or (errorHandler.buildDepError "constraints-extras"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."convertible" or (errorHandler.buildDepError "convertible"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."dependent-map" or (errorHandler.buildDepError "dependent-map"))
            (hsPkgs."dependent-sum" or (errorHandler.buildDepError "dependent-sum"))
            (hsPkgs."dependent-sum-template" or (errorHandler.buildDepError "dependent-sum-template"))
            (hsPkgs."diagrams-contrib" or (errorHandler.buildDepError "diagrams-contrib"))
            (hsPkgs."diagrams-lib" or (errorHandler.buildDepError "diagrams-lib"))
            (hsPkgs."diagrams-svg" or (errorHandler.buildDepError "diagrams-svg"))
            (hsPkgs."dimensional" or (errorHandler.buildDepError "dimensional"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
            (hsPkgs."envy" or (errorHandler.buildDepError "envy"))
            (hsPkgs."estimator" or (errorHandler.buildDepError "estimator"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."generic-data" or (errorHandler.buildDepError "generic-data"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."ghcjs-dom" or (errorHandler.buildDepError "ghcjs-dom"))
            (hsPkgs."greskell" or (errorHandler.buildDepError "greskell"))
            (hsPkgs."greskell-core" or (errorHandler.buildDepError "greskell-core"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."haxl" or (errorHandler.buildDepError "haxl"))
            (hsPkgs."higgledy" or (errorHandler.buildDepError "higgledy"))
            (hsPkgs."interpolatedstring-perl6" or (errorHandler.buildDepError "interpolatedstring-perl6"))
            (hsPkgs."jsaddle" or (errorHandler.buildDepError "jsaddle"))
            (hsPkgs."keys" or (errorHandler.buildDepError "keys"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."linear" or (errorHandler.buildDepError "linear"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."monad-bayes" or (errorHandler.buildDepError "monad-bayes"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
            (hsPkgs."one-liner" or (errorHandler.buildDepError "one-liner"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."pointed" or (errorHandler.buildDepError "pointed"))
            (hsPkgs."proto-lens" or (errorHandler.buildDepError "proto-lens"))
            (hsPkgs."proto-lens-runtime" or (errorHandler.buildDepError "proto-lens-runtime"))
            (hsPkgs."raw-strings-qq" or (errorHandler.buildDepError "raw-strings-qq"))
            (hsPkgs."regex-applicative" or (errorHandler.buildDepError "regex-applicative"))
            (hsPkgs."resource-pool" or (errorHandler.buildDepError "resource-pool"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."retry" or (errorHandler.buildDepError "retry"))
            (hsPkgs."rio" or (errorHandler.buildDepError "rio"))
            (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-client-js" or (errorHandler.buildDepError "servant-client-js"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
            (hsPkgs."streamly-bytestring" or (errorHandler.buildDepError "streamly-bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
            (hsPkgs."unagi-chan" or (errorHandler.buildDepError "unagi-chan"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vector-sized" or (errorHandler.buildDepError "vector-sized"))
            ] ++ (if !(compiler.isGhcjs && true)
            then [
              (hsPkgs."amazonka" or (errorHandler.buildDepError "amazonka"))
              (hsPkgs."amazonka-iot" or (errorHandler.buildDepError "amazonka-iot"))
              (hsPkgs."amazonka-s3" or (errorHandler.buildDepError "amazonka-s3"))
              (hsPkgs."cas-hashable" or (errorHandler.buildDepError "cas-hashable"))
              (hsPkgs."cas-store" or (errorHandler.buildDepError "cas-store"))
              (hsPkgs."composite-ekg" or (errorHandler.buildDepError "composite-ekg"))
              (hsPkgs."concat-classes" or (errorHandler.buildDepError "concat-classes"))
              (hsPkgs."concat-examples" or (errorHandler.buildDepError "concat-examples"))
              (hsPkgs."concat-graphics" or (errorHandler.buildDepError "concat-graphics"))
              (hsPkgs."concat-plugin" or (errorHandler.buildDepError "concat-plugin"))
              (hsPkgs."concurrent-dns-cache" or (errorHandler.buildDepError "concurrent-dns-cache"))
              (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
              (hsPkgs."connection" or (errorHandler.buildDepError "connection"))
              (hsPkgs."dhall" or (errorHandler.buildDepError "dhall"))
              (hsPkgs."directory-tree" or (errorHandler.buildDepError "directory-tree"))
              (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
              (hsPkgs."ekg" or (errorHandler.buildDepError "ekg"))
              (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
              (hsPkgs."fusion-plugin" or (errorHandler.buildDepError "fusion-plugin"))
              (hsPkgs."greskell-websocket" or (errorHandler.buildDepError "greskell-websocket"))
              (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
              (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
              (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
              (hsPkgs."influxdb" or (errorHandler.buildDepError "influxdb"))
              (hsPkgs."katip" or (errorHandler.buildDepError "katip"))
              (hsPkgs."net-mqtt" or (errorHandler.buildDepError "net-mqtt"))
              (hsPkgs."net-spider" or (errorHandler.buildDepError "net-spider"))
              (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
              (hsPkgs."sbv" or (errorHandler.buildDepError "sbv"))
              (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
              (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
              (hsPkgs."servant-js" or (errorHandler.buildDepError "servant-js"))
              (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
              (hsPkgs."servant-websockets" or (errorHandler.buildDepError "servant-websockets"))
              (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
              (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
              (hsPkgs."vinyl" or (errorHandler.buildDepError "vinyl"))
              (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
              (hsPkgs."wai-cors" or (errorHandler.buildDepError "wai-cors"))
              (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
              (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
              (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
              (hsPkgs."winery" or (errorHandler.buildDepError "winery"))
              (hsPkgs."x509-store" or (errorHandler.buildDepError "x509-store"))
              (hsPkgs."x509-validation" or (errorHandler.buildDepError "x509-validation"))
              ]
            else [
              (hsPkgs."ghcjs-base" or (errorHandler.buildDepError "ghcjs-base"))
              ])) ++ (pkgs.lib).optionals (!(compiler.isGhcjs && true)) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."chopaan" or (errorHandler.buildDepError "chopaan"))
            (hsPkgs."grafana" or (errorHandler.buildDepError "grafana"))
            (hsPkgs."optparse-simple" or (errorHandler.buildDepError "optparse-simple"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = if compiler.isGhcjs && true then false else true;
          hsSourceDirs = [ "app" ];
          mainPath = ([ "DashGen.hs" ] ++ (if !(compiler.isGhcjs && true)
            then [ "" ] ++ [ "" ]
            else [ "" ])) ++ [ "" ];
          };
        "hydrate" = {
          depends = ([
            (hsPkgs."Shpadoinkle" or (errorHandler.buildDepError "Shpadoinkle"))
            (hsPkgs."Shpadoinkle-backend-pardiff" or (errorHandler.buildDepError "Shpadoinkle-backend-pardiff"))
            (hsPkgs."Shpadoinkle-backend-snabbdom" or (errorHandler.buildDepError "Shpadoinkle-backend-snabbdom"))
            (hsPkgs."Shpadoinkle-console" or (errorHandler.buildDepError "Shpadoinkle-console"))
            (hsPkgs."Shpadoinkle-html" or (errorHandler.buildDepError "Shpadoinkle-html"))
            (hsPkgs."Shpadoinkle-lens" or (errorHandler.buildDepError "Shpadoinkle-lens"))
            (hsPkgs."Shpadoinkle-router" or (errorHandler.buildDepError "Shpadoinkle-router"))
            (hsPkgs."Shpadoinkle-template" or (errorHandler.buildDepError "Shpadoinkle-template"))
            (hsPkgs."Shpadoinkle-widgets" or (errorHandler.buildDepError "Shpadoinkle-widgets"))
            (hsPkgs."ad" or (errorHandler.buildDepError "ad"))
            (hsPkgs."adjunctions" or (errorHandler.buildDepError "adjunctions"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."algebraic-graphs" or (errorHandler.buildDepError "algebraic-graphs"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64" or (errorHandler.buildDepError "base64"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."clay" or (errorHandler.buildDepError "clay"))
            (hsPkgs."colour" or (errorHandler.buildDepError "colour"))
            (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
            (hsPkgs."compensated" or (errorHandler.buildDepError "compensated"))
            (hsPkgs."constraints-extras" or (errorHandler.buildDepError "constraints-extras"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."convertible" or (errorHandler.buildDepError "convertible"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."dependent-map" or (errorHandler.buildDepError "dependent-map"))
            (hsPkgs."dependent-sum" or (errorHandler.buildDepError "dependent-sum"))
            (hsPkgs."dependent-sum-template" or (errorHandler.buildDepError "dependent-sum-template"))
            (hsPkgs."diagrams-contrib" or (errorHandler.buildDepError "diagrams-contrib"))
            (hsPkgs."diagrams-lib" or (errorHandler.buildDepError "diagrams-lib"))
            (hsPkgs."diagrams-svg" or (errorHandler.buildDepError "diagrams-svg"))
            (hsPkgs."dimensional" or (errorHandler.buildDepError "dimensional"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
            (hsPkgs."envy" or (errorHandler.buildDepError "envy"))
            (hsPkgs."estimator" or (errorHandler.buildDepError "estimator"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."generic-data" or (errorHandler.buildDepError "generic-data"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."ghcjs-dom" or (errorHandler.buildDepError "ghcjs-dom"))
            (hsPkgs."greskell" or (errorHandler.buildDepError "greskell"))
            (hsPkgs."greskell-core" or (errorHandler.buildDepError "greskell-core"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."haxl" or (errorHandler.buildDepError "haxl"))
            (hsPkgs."higgledy" or (errorHandler.buildDepError "higgledy"))
            (hsPkgs."interpolatedstring-perl6" or (errorHandler.buildDepError "interpolatedstring-perl6"))
            (hsPkgs."jsaddle" or (errorHandler.buildDepError "jsaddle"))
            (hsPkgs."keys" or (errorHandler.buildDepError "keys"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."linear" or (errorHandler.buildDepError "linear"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."monad-bayes" or (errorHandler.buildDepError "monad-bayes"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
            (hsPkgs."one-liner" or (errorHandler.buildDepError "one-liner"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."pointed" or (errorHandler.buildDepError "pointed"))
            (hsPkgs."proto-lens" or (errorHandler.buildDepError "proto-lens"))
            (hsPkgs."proto-lens-runtime" or (errorHandler.buildDepError "proto-lens-runtime"))
            (hsPkgs."raw-strings-qq" or (errorHandler.buildDepError "raw-strings-qq"))
            (hsPkgs."regex-applicative" or (errorHandler.buildDepError "regex-applicative"))
            (hsPkgs."resource-pool" or (errorHandler.buildDepError "resource-pool"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."retry" or (errorHandler.buildDepError "retry"))
            (hsPkgs."rio" or (errorHandler.buildDepError "rio"))
            (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-client-js" or (errorHandler.buildDepError "servant-client-js"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
            (hsPkgs."streamly-bytestring" or (errorHandler.buildDepError "streamly-bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
            (hsPkgs."unagi-chan" or (errorHandler.buildDepError "unagi-chan"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vector-sized" or (errorHandler.buildDepError "vector-sized"))
            ] ++ (if !(compiler.isGhcjs && true)
            then [
              (hsPkgs."amazonka" or (errorHandler.buildDepError "amazonka"))
              (hsPkgs."amazonka-iot" or (errorHandler.buildDepError "amazonka-iot"))
              (hsPkgs."amazonka-s3" or (errorHandler.buildDepError "amazonka-s3"))
              (hsPkgs."cas-hashable" or (errorHandler.buildDepError "cas-hashable"))
              (hsPkgs."cas-store" or (errorHandler.buildDepError "cas-store"))
              (hsPkgs."composite-ekg" or (errorHandler.buildDepError "composite-ekg"))
              (hsPkgs."concat-classes" or (errorHandler.buildDepError "concat-classes"))
              (hsPkgs."concat-examples" or (errorHandler.buildDepError "concat-examples"))
              (hsPkgs."concat-graphics" or (errorHandler.buildDepError "concat-graphics"))
              (hsPkgs."concat-plugin" or (errorHandler.buildDepError "concat-plugin"))
              (hsPkgs."concurrent-dns-cache" or (errorHandler.buildDepError "concurrent-dns-cache"))
              (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
              (hsPkgs."connection" or (errorHandler.buildDepError "connection"))
              (hsPkgs."dhall" or (errorHandler.buildDepError "dhall"))
              (hsPkgs."directory-tree" or (errorHandler.buildDepError "directory-tree"))
              (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
              (hsPkgs."ekg" or (errorHandler.buildDepError "ekg"))
              (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
              (hsPkgs."fusion-plugin" or (errorHandler.buildDepError "fusion-plugin"))
              (hsPkgs."greskell-websocket" or (errorHandler.buildDepError "greskell-websocket"))
              (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
              (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
              (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
              (hsPkgs."influxdb" or (errorHandler.buildDepError "influxdb"))
              (hsPkgs."katip" or (errorHandler.buildDepError "katip"))
              (hsPkgs."net-mqtt" or (errorHandler.buildDepError "net-mqtt"))
              (hsPkgs."net-spider" or (errorHandler.buildDepError "net-spider"))
              (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
              (hsPkgs."sbv" or (errorHandler.buildDepError "sbv"))
              (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
              (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
              (hsPkgs."servant-js" or (errorHandler.buildDepError "servant-js"))
              (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
              (hsPkgs."servant-websockets" or (errorHandler.buildDepError "servant-websockets"))
              (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
              (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
              (hsPkgs."vinyl" or (errorHandler.buildDepError "vinyl"))
              (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
              (hsPkgs."wai-cors" or (errorHandler.buildDepError "wai-cors"))
              (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
              (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
              (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
              (hsPkgs."winery" or (errorHandler.buildDepError "winery"))
              (hsPkgs."x509-store" or (errorHandler.buildDepError "x509-store"))
              (hsPkgs."x509-validation" or (errorHandler.buildDepError "x509-validation"))
              ]
            else [
              (hsPkgs."ghcjs-base" or (errorHandler.buildDepError "ghcjs-base"))
              ])) ++ (pkgs.lib).optionals (!(compiler.isGhcjs && true)) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."chopaan" or (errorHandler.buildDepError "chopaan"))
            (hsPkgs."optparse-simple" or (errorHandler.buildDepError "optparse-simple"))
            ];
          buildable = if !(compiler.isGhcjs && true) then true else false;
          modules = [ "Paths_chopaan" ];
          hsSourceDirs = [ "app" ];
          mainPath = ([ "Hydrate.hs" ] ++ (if !(compiler.isGhcjs && true)
            then [ "" ] ++ [ "" ]
            else [ "" ])) ++ (if !(compiler.isGhcjs && true)
            then [ "" ] ++ [ "" ]
            else [ "" ]);
          };
        "kbtzim" = {
          depends = ([
            (hsPkgs."Shpadoinkle" or (errorHandler.buildDepError "Shpadoinkle"))
            (hsPkgs."Shpadoinkle-backend-pardiff" or (errorHandler.buildDepError "Shpadoinkle-backend-pardiff"))
            (hsPkgs."Shpadoinkle-backend-snabbdom" or (errorHandler.buildDepError "Shpadoinkle-backend-snabbdom"))
            (hsPkgs."Shpadoinkle-console" or (errorHandler.buildDepError "Shpadoinkle-console"))
            (hsPkgs."Shpadoinkle-html" or (errorHandler.buildDepError "Shpadoinkle-html"))
            (hsPkgs."Shpadoinkle-lens" or (errorHandler.buildDepError "Shpadoinkle-lens"))
            (hsPkgs."Shpadoinkle-router" or (errorHandler.buildDepError "Shpadoinkle-router"))
            (hsPkgs."Shpadoinkle-template" or (errorHandler.buildDepError "Shpadoinkle-template"))
            (hsPkgs."Shpadoinkle-widgets" or (errorHandler.buildDepError "Shpadoinkle-widgets"))
            (hsPkgs."ad" or (errorHandler.buildDepError "ad"))
            (hsPkgs."adjunctions" or (errorHandler.buildDepError "adjunctions"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."algebraic-graphs" or (errorHandler.buildDepError "algebraic-graphs"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64" or (errorHandler.buildDepError "base64"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."clay" or (errorHandler.buildDepError "clay"))
            (hsPkgs."colour" or (errorHandler.buildDepError "colour"))
            (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
            (hsPkgs."compensated" or (errorHandler.buildDepError "compensated"))
            (hsPkgs."constraints-extras" or (errorHandler.buildDepError "constraints-extras"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."convertible" or (errorHandler.buildDepError "convertible"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."dependent-map" or (errorHandler.buildDepError "dependent-map"))
            (hsPkgs."dependent-sum" or (errorHandler.buildDepError "dependent-sum"))
            (hsPkgs."dependent-sum-template" or (errorHandler.buildDepError "dependent-sum-template"))
            (hsPkgs."diagrams-contrib" or (errorHandler.buildDepError "diagrams-contrib"))
            (hsPkgs."diagrams-lib" or (errorHandler.buildDepError "diagrams-lib"))
            (hsPkgs."diagrams-svg" or (errorHandler.buildDepError "diagrams-svg"))
            (hsPkgs."dimensional" or (errorHandler.buildDepError "dimensional"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
            (hsPkgs."envy" or (errorHandler.buildDepError "envy"))
            (hsPkgs."estimator" or (errorHandler.buildDepError "estimator"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."generic-data" or (errorHandler.buildDepError "generic-data"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."ghcjs-dom" or (errorHandler.buildDepError "ghcjs-dom"))
            (hsPkgs."greskell" or (errorHandler.buildDepError "greskell"))
            (hsPkgs."greskell-core" or (errorHandler.buildDepError "greskell-core"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."haxl" or (errorHandler.buildDepError "haxl"))
            (hsPkgs."higgledy" or (errorHandler.buildDepError "higgledy"))
            (hsPkgs."interpolatedstring-perl6" or (errorHandler.buildDepError "interpolatedstring-perl6"))
            (hsPkgs."jsaddle" or (errorHandler.buildDepError "jsaddle"))
            (hsPkgs."keys" or (errorHandler.buildDepError "keys"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."linear" or (errorHandler.buildDepError "linear"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."monad-bayes" or (errorHandler.buildDepError "monad-bayes"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
            (hsPkgs."one-liner" or (errorHandler.buildDepError "one-liner"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."pointed" or (errorHandler.buildDepError "pointed"))
            (hsPkgs."proto-lens" or (errorHandler.buildDepError "proto-lens"))
            (hsPkgs."proto-lens-runtime" or (errorHandler.buildDepError "proto-lens-runtime"))
            (hsPkgs."raw-strings-qq" or (errorHandler.buildDepError "raw-strings-qq"))
            (hsPkgs."regex-applicative" or (errorHandler.buildDepError "regex-applicative"))
            (hsPkgs."resource-pool" or (errorHandler.buildDepError "resource-pool"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."retry" or (errorHandler.buildDepError "retry"))
            (hsPkgs."rio" or (errorHandler.buildDepError "rio"))
            (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-client-js" or (errorHandler.buildDepError "servant-client-js"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
            (hsPkgs."streamly-bytestring" or (errorHandler.buildDepError "streamly-bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
            (hsPkgs."unagi-chan" or (errorHandler.buildDepError "unagi-chan"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vector-sized" or (errorHandler.buildDepError "vector-sized"))
            ] ++ (if !(compiler.isGhcjs && true)
            then [
              (hsPkgs."amazonka" or (errorHandler.buildDepError "amazonka"))
              (hsPkgs."amazonka-iot" or (errorHandler.buildDepError "amazonka-iot"))
              (hsPkgs."amazonka-s3" or (errorHandler.buildDepError "amazonka-s3"))
              (hsPkgs."cas-hashable" or (errorHandler.buildDepError "cas-hashable"))
              (hsPkgs."cas-store" or (errorHandler.buildDepError "cas-store"))
              (hsPkgs."composite-ekg" or (errorHandler.buildDepError "composite-ekg"))
              (hsPkgs."concat-classes" or (errorHandler.buildDepError "concat-classes"))
              (hsPkgs."concat-examples" or (errorHandler.buildDepError "concat-examples"))
              (hsPkgs."concat-graphics" or (errorHandler.buildDepError "concat-graphics"))
              (hsPkgs."concat-plugin" or (errorHandler.buildDepError "concat-plugin"))
              (hsPkgs."concurrent-dns-cache" or (errorHandler.buildDepError "concurrent-dns-cache"))
              (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
              (hsPkgs."connection" or (errorHandler.buildDepError "connection"))
              (hsPkgs."dhall" or (errorHandler.buildDepError "dhall"))
              (hsPkgs."directory-tree" or (errorHandler.buildDepError "directory-tree"))
              (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
              (hsPkgs."ekg" or (errorHandler.buildDepError "ekg"))
              (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
              (hsPkgs."fusion-plugin" or (errorHandler.buildDepError "fusion-plugin"))
              (hsPkgs."greskell-websocket" or (errorHandler.buildDepError "greskell-websocket"))
              (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
              (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
              (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
              (hsPkgs."influxdb" or (errorHandler.buildDepError "influxdb"))
              (hsPkgs."katip" or (errorHandler.buildDepError "katip"))
              (hsPkgs."net-mqtt" or (errorHandler.buildDepError "net-mqtt"))
              (hsPkgs."net-spider" or (errorHandler.buildDepError "net-spider"))
              (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
              (hsPkgs."sbv" or (errorHandler.buildDepError "sbv"))
              (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
              (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
              (hsPkgs."servant-js" or (errorHandler.buildDepError "servant-js"))
              (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
              (hsPkgs."servant-websockets" or (errorHandler.buildDepError "servant-websockets"))
              (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
              (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
              (hsPkgs."vinyl" or (errorHandler.buildDepError "vinyl"))
              (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
              (hsPkgs."wai-cors" or (errorHandler.buildDepError "wai-cors"))
              (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
              (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
              (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
              (hsPkgs."winery" or (errorHandler.buildDepError "winery"))
              (hsPkgs."x509-store" or (errorHandler.buildDepError "x509-store"))
              (hsPkgs."x509-validation" or (errorHandler.buildDepError "x509-validation"))
              ]
            else [
              (hsPkgs."ghcjs-base" or (errorHandler.buildDepError "ghcjs-base"))
              ])) ++ (pkgs.lib).optionals (!(compiler.isGhcjs && true)) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."chopaan" or (errorHandler.buildDepError "chopaan"))
            (hsPkgs."optparse-simple" or (errorHandler.buildDepError "optparse-simple"))
            ];
          buildable = if !(compiler.isGhcjs && true) then true else false;
          modules = [ "Paths_chopaan" ];
          hsSourceDirs = [ "app" ];
          mainPath = ([ "Main.hs" ] ++ (if !(compiler.isGhcjs && true)
            then [ "" ] ++ [ "" ]
            else [ "" ])) ++ (if !(compiler.isGhcjs && true)
            then [ "" ] ++ [ "" ]
            else [ "" ]);
          };
        "server" = {
          depends = ([
            (hsPkgs."Shpadoinkle" or (errorHandler.buildDepError "Shpadoinkle"))
            (hsPkgs."Shpadoinkle-backend-pardiff" or (errorHandler.buildDepError "Shpadoinkle-backend-pardiff"))
            (hsPkgs."Shpadoinkle-backend-snabbdom" or (errorHandler.buildDepError "Shpadoinkle-backend-snabbdom"))
            (hsPkgs."Shpadoinkle-console" or (errorHandler.buildDepError "Shpadoinkle-console"))
            (hsPkgs."Shpadoinkle-html" or (errorHandler.buildDepError "Shpadoinkle-html"))
            (hsPkgs."Shpadoinkle-lens" or (errorHandler.buildDepError "Shpadoinkle-lens"))
            (hsPkgs."Shpadoinkle-router" or (errorHandler.buildDepError "Shpadoinkle-router"))
            (hsPkgs."Shpadoinkle-template" or (errorHandler.buildDepError "Shpadoinkle-template"))
            (hsPkgs."Shpadoinkle-widgets" or (errorHandler.buildDepError "Shpadoinkle-widgets"))
            (hsPkgs."ad" or (errorHandler.buildDepError "ad"))
            (hsPkgs."adjunctions" or (errorHandler.buildDepError "adjunctions"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."algebraic-graphs" or (errorHandler.buildDepError "algebraic-graphs"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64" or (errorHandler.buildDepError "base64"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."clay" or (errorHandler.buildDepError "clay"))
            (hsPkgs."colour" or (errorHandler.buildDepError "colour"))
            (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
            (hsPkgs."compensated" or (errorHandler.buildDepError "compensated"))
            (hsPkgs."constraints-extras" or (errorHandler.buildDepError "constraints-extras"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."convertible" or (errorHandler.buildDepError "convertible"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."dependent-map" or (errorHandler.buildDepError "dependent-map"))
            (hsPkgs."dependent-sum" or (errorHandler.buildDepError "dependent-sum"))
            (hsPkgs."dependent-sum-template" or (errorHandler.buildDepError "dependent-sum-template"))
            (hsPkgs."diagrams-contrib" or (errorHandler.buildDepError "diagrams-contrib"))
            (hsPkgs."diagrams-lib" or (errorHandler.buildDepError "diagrams-lib"))
            (hsPkgs."diagrams-svg" or (errorHandler.buildDepError "diagrams-svg"))
            (hsPkgs."dimensional" or (errorHandler.buildDepError "dimensional"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
            (hsPkgs."envy" or (errorHandler.buildDepError "envy"))
            (hsPkgs."estimator" or (errorHandler.buildDepError "estimator"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."generic-data" or (errorHandler.buildDepError "generic-data"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."ghcjs-dom" or (errorHandler.buildDepError "ghcjs-dom"))
            (hsPkgs."greskell" or (errorHandler.buildDepError "greskell"))
            (hsPkgs."greskell-core" or (errorHandler.buildDepError "greskell-core"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."haxl" or (errorHandler.buildDepError "haxl"))
            (hsPkgs."higgledy" or (errorHandler.buildDepError "higgledy"))
            (hsPkgs."interpolatedstring-perl6" or (errorHandler.buildDepError "interpolatedstring-perl6"))
            (hsPkgs."jsaddle" or (errorHandler.buildDepError "jsaddle"))
            (hsPkgs."keys" or (errorHandler.buildDepError "keys"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."linear" or (errorHandler.buildDepError "linear"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."monad-bayes" or (errorHandler.buildDepError "monad-bayes"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
            (hsPkgs."one-liner" or (errorHandler.buildDepError "one-liner"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."pointed" or (errorHandler.buildDepError "pointed"))
            (hsPkgs."proto-lens" or (errorHandler.buildDepError "proto-lens"))
            (hsPkgs."proto-lens-runtime" or (errorHandler.buildDepError "proto-lens-runtime"))
            (hsPkgs."raw-strings-qq" or (errorHandler.buildDepError "raw-strings-qq"))
            (hsPkgs."regex-applicative" or (errorHandler.buildDepError "regex-applicative"))
            (hsPkgs."resource-pool" or (errorHandler.buildDepError "resource-pool"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."retry" or (errorHandler.buildDepError "retry"))
            (hsPkgs."rio" or (errorHandler.buildDepError "rio"))
            (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-client-js" or (errorHandler.buildDepError "servant-client-js"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
            (hsPkgs."streamly-bytestring" or (errorHandler.buildDepError "streamly-bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
            (hsPkgs."unagi-chan" or (errorHandler.buildDepError "unagi-chan"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vector-sized" or (errorHandler.buildDepError "vector-sized"))
            ] ++ (if !(compiler.isGhcjs && true)
            then [
              (hsPkgs."amazonka" or (errorHandler.buildDepError "amazonka"))
              (hsPkgs."amazonka-iot" or (errorHandler.buildDepError "amazonka-iot"))
              (hsPkgs."amazonka-s3" or (errorHandler.buildDepError "amazonka-s3"))
              (hsPkgs."cas-hashable" or (errorHandler.buildDepError "cas-hashable"))
              (hsPkgs."cas-store" or (errorHandler.buildDepError "cas-store"))
              (hsPkgs."composite-ekg" or (errorHandler.buildDepError "composite-ekg"))
              (hsPkgs."concat-classes" or (errorHandler.buildDepError "concat-classes"))
              (hsPkgs."concat-examples" or (errorHandler.buildDepError "concat-examples"))
              (hsPkgs."concat-graphics" or (errorHandler.buildDepError "concat-graphics"))
              (hsPkgs."concat-plugin" or (errorHandler.buildDepError "concat-plugin"))
              (hsPkgs."concurrent-dns-cache" or (errorHandler.buildDepError "concurrent-dns-cache"))
              (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
              (hsPkgs."connection" or (errorHandler.buildDepError "connection"))
              (hsPkgs."dhall" or (errorHandler.buildDepError "dhall"))
              (hsPkgs."directory-tree" or (errorHandler.buildDepError "directory-tree"))
              (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
              (hsPkgs."ekg" or (errorHandler.buildDepError "ekg"))
              (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
              (hsPkgs."fusion-plugin" or (errorHandler.buildDepError "fusion-plugin"))
              (hsPkgs."greskell-websocket" or (errorHandler.buildDepError "greskell-websocket"))
              (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
              (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
              (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
              (hsPkgs."influxdb" or (errorHandler.buildDepError "influxdb"))
              (hsPkgs."katip" or (errorHandler.buildDepError "katip"))
              (hsPkgs."net-mqtt" or (errorHandler.buildDepError "net-mqtt"))
              (hsPkgs."net-spider" or (errorHandler.buildDepError "net-spider"))
              (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
              (hsPkgs."sbv" or (errorHandler.buildDepError "sbv"))
              (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
              (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
              (hsPkgs."servant-js" or (errorHandler.buildDepError "servant-js"))
              (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
              (hsPkgs."servant-websockets" or (errorHandler.buildDepError "servant-websockets"))
              (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
              (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
              (hsPkgs."vinyl" or (errorHandler.buildDepError "vinyl"))
              (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
              (hsPkgs."wai-cors" or (errorHandler.buildDepError "wai-cors"))
              (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
              (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
              (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
              (hsPkgs."winery" or (errorHandler.buildDepError "winery"))
              (hsPkgs."x509-store" or (errorHandler.buildDepError "x509-store"))
              (hsPkgs."x509-validation" or (errorHandler.buildDepError "x509-validation"))
              ]
            else [
              (hsPkgs."ghcjs-base" or (errorHandler.buildDepError "ghcjs-base"))
              ])) ++ (pkgs.lib).optionals (!(compiler.isGhcjs && true)) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."chopaan" or (errorHandler.buildDepError "chopaan"))
            (hsPkgs."optparse-simple" or (errorHandler.buildDepError "optparse-simple"))
            ];
          buildable = if compiler.isGhcjs && true then false else true;
          modules = (pkgs.lib).optional (!(compiler.isGhcjs && true)) "Paths_chopaan";
          hsSourceDirs = [ "app" ];
          mainPath = ([ "Server.hs" ] ++ (if !(compiler.isGhcjs && true)
            then [ "" ] ++ [ "" ]
            else [ "" ])) ++ (if compiler.isGhcjs && true
            then [ "" ]
            else [ "" ] ++ [ "" ]);
          };
        };
      tests = {
        "chopaan-test" = {
          depends = ([
            (hsPkgs."Shpadoinkle" or (errorHandler.buildDepError "Shpadoinkle"))
            (hsPkgs."Shpadoinkle-backend-pardiff" or (errorHandler.buildDepError "Shpadoinkle-backend-pardiff"))
            (hsPkgs."Shpadoinkle-backend-snabbdom" or (errorHandler.buildDepError "Shpadoinkle-backend-snabbdom"))
            (hsPkgs."Shpadoinkle-console" or (errorHandler.buildDepError "Shpadoinkle-console"))
            (hsPkgs."Shpadoinkle-html" or (errorHandler.buildDepError "Shpadoinkle-html"))
            (hsPkgs."Shpadoinkle-lens" or (errorHandler.buildDepError "Shpadoinkle-lens"))
            (hsPkgs."Shpadoinkle-router" or (errorHandler.buildDepError "Shpadoinkle-router"))
            (hsPkgs."Shpadoinkle-template" or (errorHandler.buildDepError "Shpadoinkle-template"))
            (hsPkgs."Shpadoinkle-widgets" or (errorHandler.buildDepError "Shpadoinkle-widgets"))
            (hsPkgs."ad" or (errorHandler.buildDepError "ad"))
            (hsPkgs."adjunctions" or (errorHandler.buildDepError "adjunctions"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."algebraic-graphs" or (errorHandler.buildDepError "algebraic-graphs"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64" or (errorHandler.buildDepError "base64"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."clay" or (errorHandler.buildDepError "clay"))
            (hsPkgs."colour" or (errorHandler.buildDepError "colour"))
            (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
            (hsPkgs."compensated" or (errorHandler.buildDepError "compensated"))
            (hsPkgs."constraints-extras" or (errorHandler.buildDepError "constraints-extras"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."convertible" or (errorHandler.buildDepError "convertible"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."dependent-map" or (errorHandler.buildDepError "dependent-map"))
            (hsPkgs."dependent-sum" or (errorHandler.buildDepError "dependent-sum"))
            (hsPkgs."dependent-sum-template" or (errorHandler.buildDepError "dependent-sum-template"))
            (hsPkgs."diagrams-contrib" or (errorHandler.buildDepError "diagrams-contrib"))
            (hsPkgs."diagrams-lib" or (errorHandler.buildDepError "diagrams-lib"))
            (hsPkgs."diagrams-svg" or (errorHandler.buildDepError "diagrams-svg"))
            (hsPkgs."dimensional" or (errorHandler.buildDepError "dimensional"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
            (hsPkgs."envy" or (errorHandler.buildDepError "envy"))
            (hsPkgs."estimator" or (errorHandler.buildDepError "estimator"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."generic-data" or (errorHandler.buildDepError "generic-data"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."ghcjs-dom" or (errorHandler.buildDepError "ghcjs-dom"))
            (hsPkgs."greskell" or (errorHandler.buildDepError "greskell"))
            (hsPkgs."greskell-core" or (errorHandler.buildDepError "greskell-core"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."haxl" or (errorHandler.buildDepError "haxl"))
            (hsPkgs."higgledy" or (errorHandler.buildDepError "higgledy"))
            (hsPkgs."interpolatedstring-perl6" or (errorHandler.buildDepError "interpolatedstring-perl6"))
            (hsPkgs."jsaddle" or (errorHandler.buildDepError "jsaddle"))
            (hsPkgs."keys" or (errorHandler.buildDepError "keys"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."linear" or (errorHandler.buildDepError "linear"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."monad-bayes" or (errorHandler.buildDepError "monad-bayes"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
            (hsPkgs."one-liner" or (errorHandler.buildDepError "one-liner"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."pointed" or (errorHandler.buildDepError "pointed"))
            (hsPkgs."proto-lens" or (errorHandler.buildDepError "proto-lens"))
            (hsPkgs."proto-lens-runtime" or (errorHandler.buildDepError "proto-lens-runtime"))
            (hsPkgs."raw-strings-qq" or (errorHandler.buildDepError "raw-strings-qq"))
            (hsPkgs."regex-applicative" or (errorHandler.buildDepError "regex-applicative"))
            (hsPkgs."resource-pool" or (errorHandler.buildDepError "resource-pool"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."retry" or (errorHandler.buildDepError "retry"))
            (hsPkgs."rio" or (errorHandler.buildDepError "rio"))
            (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-client-js" or (errorHandler.buildDepError "servant-client-js"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
            (hsPkgs."streamly-bytestring" or (errorHandler.buildDepError "streamly-bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
            (hsPkgs."unagi-chan" or (errorHandler.buildDepError "unagi-chan"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vector-sized" or (errorHandler.buildDepError "vector-sized"))
            ] ++ (if !(compiler.isGhcjs && true)
            then [
              (hsPkgs."amazonka" or (errorHandler.buildDepError "amazonka"))
              (hsPkgs."amazonka-iot" or (errorHandler.buildDepError "amazonka-iot"))
              (hsPkgs."amazonka-s3" or (errorHandler.buildDepError "amazonka-s3"))
              (hsPkgs."cas-hashable" or (errorHandler.buildDepError "cas-hashable"))
              (hsPkgs."cas-store" or (errorHandler.buildDepError "cas-store"))
              (hsPkgs."composite-ekg" or (errorHandler.buildDepError "composite-ekg"))
              (hsPkgs."concat-classes" or (errorHandler.buildDepError "concat-classes"))
              (hsPkgs."concat-examples" or (errorHandler.buildDepError "concat-examples"))
              (hsPkgs."concat-graphics" or (errorHandler.buildDepError "concat-graphics"))
              (hsPkgs."concat-plugin" or (errorHandler.buildDepError "concat-plugin"))
              (hsPkgs."concurrent-dns-cache" or (errorHandler.buildDepError "concurrent-dns-cache"))
              (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
              (hsPkgs."connection" or (errorHandler.buildDepError "connection"))
              (hsPkgs."dhall" or (errorHandler.buildDepError "dhall"))
              (hsPkgs."directory-tree" or (errorHandler.buildDepError "directory-tree"))
              (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
              (hsPkgs."ekg" or (errorHandler.buildDepError "ekg"))
              (hsPkgs."ekg-core" or (errorHandler.buildDepError "ekg-core"))
              (hsPkgs."fusion-plugin" or (errorHandler.buildDepError "fusion-plugin"))
              (hsPkgs."greskell-websocket" or (errorHandler.buildDepError "greskell-websocket"))
              (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
              (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
              (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
              (hsPkgs."influxdb" or (errorHandler.buildDepError "influxdb"))
              (hsPkgs."katip" or (errorHandler.buildDepError "katip"))
              (hsPkgs."net-mqtt" or (errorHandler.buildDepError "net-mqtt"))
              (hsPkgs."net-spider" or (errorHandler.buildDepError "net-spider"))
              (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
              (hsPkgs."sbv" or (errorHandler.buildDepError "sbv"))
              (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
              (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
              (hsPkgs."servant-js" or (errorHandler.buildDepError "servant-js"))
              (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
              (hsPkgs."servant-websockets" or (errorHandler.buildDepError "servant-websockets"))
              (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
              (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
              (hsPkgs."vinyl" or (errorHandler.buildDepError "vinyl"))
              (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
              (hsPkgs."wai-cors" or (errorHandler.buildDepError "wai-cors"))
              (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
              (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
              (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
              (hsPkgs."winery" or (errorHandler.buildDepError "winery"))
              (hsPkgs."x509-store" or (errorHandler.buildDepError "x509-store"))
              (hsPkgs."x509-validation" or (errorHandler.buildDepError "x509-validation"))
              ]
            else [
              (hsPkgs."ghcjs-base" or (errorHandler.buildDepError "ghcjs-base"))
              ])) ++ (if compiler.isGhcjs && true
            then [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ]
            else [
              (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
              (hsPkgs."checkers" or (errorHandler.buildDepError "checkers"))
              (hsPkgs."chopaan" or (errorHandler.buildDepError "chopaan"))
              (hsPkgs."generic-arbitrary" or (errorHandler.buildDepError "generic-arbitrary"))
              (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
              (hsPkgs."hspec-golden-aeson" or (errorHandler.buildDepError "hspec-golden-aeson"))
              (hsPkgs."hspec-wai" or (errorHandler.buildDepError "hspec-wai"))
              (hsPkgs."proto-lens-arbitrary" or (errorHandler.buildDepError "proto-lens-arbitrary"))
              (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
              (hsPkgs."quickspec" or (errorHandler.buildDepError "quickspec"))
              (hsPkgs."testcontainers" or (errorHandler.buildDepError "testcontainers"))
              ]);
          build-tools = (pkgs.lib).optional (!(compiler.isGhcjs && true)) (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")));
          buildable = if compiler.isGhcjs && true then false else true;
          modules = [ "Paths_chopaan" ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }) // {
    cabal-generator = "hpack";
    }