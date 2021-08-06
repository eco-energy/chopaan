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
    flags = {};
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
      extraSrcFiles = [ "README.md" "ChangeLog.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."SVGFonts" or (errorHandler.buildDepError "SVGFonts"))
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
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base64" or (errorHandler.buildDepError "base64"))
          (hsPkgs."beam-core" or (errorHandler.buildDepError "beam-core"))
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
          (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."ghcjs-dom" or (errorHandler.buildDepError "ghcjs-dom"))
          (hsPkgs."greskell" or (errorHandler.buildDepError "greskell"))
          (hsPkgs."greskell-core" or (errorHandler.buildDepError "greskell-core"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."haxl" or (errorHandler.buildDepError "haxl"))
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
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."servant-client-js" or (errorHandler.buildDepError "servant-client-js"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
          (hsPkgs."unagi-chan" or (errorHandler.buildDepError "unagi-chan"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."vector-sized" or (errorHandler.buildDepError "vector-sized"))
          ] ++ (if !(compiler.isGhcjs && true)
          then [
            (hsPkgs."amazonka" or (errorHandler.buildDepError "amazonka"))
            (hsPkgs."amazonka-iot" or (errorHandler.buildDepError "amazonka-iot"))
            (hsPkgs."amazonka-s3" or (errorHandler.buildDepError "amazonka-s3"))
            (hsPkgs."beam-migrate" or (errorHandler.buildDepError "beam-migrate"))
            (hsPkgs."beam-postgres" or (errorHandler.buildDepError "beam-postgres"))
            (hsPkgs."blaze-markup" or (errorHandler.buildDepError "blaze-markup"))
            (hsPkgs."concat-classes" or (errorHandler.buildDepError "concat-classes"))
            (hsPkgs."concat-examples" or (errorHandler.buildDepError "concat-examples"))
            (hsPkgs."concat-graphics" or (errorHandler.buildDepError "concat-graphics"))
            (hsPkgs."concat-plugin" or (errorHandler.buildDepError "concat-plugin"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."connection" or (errorHandler.buildDepError "connection"))
            (hsPkgs."dhall" or (errorHandler.buildDepError "dhall"))
            (hsPkgs."fusion-plugin" or (errorHandler.buildDepError "fusion-plugin"))
            (hsPkgs."greskell-websocket" or (errorHandler.buildDepError "greskell-websocket"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."katip" or (errorHandler.buildDepError "katip"))
            (hsPkgs."net-mqtt" or (errorHandler.buildDepError "net-mqtt"))
            (hsPkgs."net-spider" or (errorHandler.buildDepError "net-spider"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."postgresql-simple" or (errorHandler.buildDepError "postgresql-simple"))
            (hsPkgs."sbv" or (errorHandler.buildDepError "sbv"))
            (hsPkgs."servant-blaze" or (errorHandler.buildDepError "servant-blaze"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."servant-js" or (errorHandler.buildDepError "servant-js"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."servant-websockets" or (errorHandler.buildDepError "servant-websockets"))
            (hsPkgs."suavemente" or (errorHandler.buildDepError "suavemente"))
            (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."wai-cors" or (errorHandler.buildDepError "wai-cors"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
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
            "Chopaan/Kibbutz/KbtzId"
            "Chopaan/Kibbutz/KbtzimT"
            "Chopaan/Kibbutz/Kibbutz"
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
            "Chopaan/Comm/Socket"
            "Chopaan/DB"
            "Chopaan/DB/Nodes"
            "Chopaan/DB/Sensors"
            "Chopaan/Graph/Kbtz"
            "Chopaan/Graph/Spider"
            "Chopaan/Graph/VI"
            "Chopaan/Haxl/Kbtz"
            "Chopaan/Kibbutz"
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
            "Kbtz"
            "Proto/NodeMessageSchema/NodeMessages"
            "Proto/NodeMessageSchema/NodeMessages_Fields"
            "Servant/Streamly"
            "SimNode"
            "Streamly/Binary"
            "TxModel"
            ]
          else [
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
            "Chopaan/Kibbutz/KbtzimT"
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
            "Servant/Streamly"
            "Proto/NodeMessageSchema/NodeMessages"
            "Proto/NodeMessageSchema/NodeMessages_Fields"
            ]);
        hsSourceDirs = [
          "src"
          ] ++ (pkgs.lib).optional (!(!(compiler.isGhcjs && true))) "src";
        };
      exes = {
        "dev" = {
          depends = ([
            (hsPkgs."SVGFonts" or (errorHandler.buildDepError "SVGFonts"))
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
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64" or (errorHandler.buildDepError "base64"))
            (hsPkgs."beam-core" or (errorHandler.buildDepError "beam-core"))
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
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."ghcjs-dom" or (errorHandler.buildDepError "ghcjs-dom"))
            (hsPkgs."greskell" or (errorHandler.buildDepError "greskell"))
            (hsPkgs."greskell-core" or (errorHandler.buildDepError "greskell-core"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."haxl" or (errorHandler.buildDepError "haxl"))
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
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-client-js" or (errorHandler.buildDepError "servant-client-js"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
            (hsPkgs."unagi-chan" or (errorHandler.buildDepError "unagi-chan"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vector-sized" or (errorHandler.buildDepError "vector-sized"))
            ] ++ (if !(compiler.isGhcjs && true)
            then [
              (hsPkgs."amazonka" or (errorHandler.buildDepError "amazonka"))
              (hsPkgs."amazonka-iot" or (errorHandler.buildDepError "amazonka-iot"))
              (hsPkgs."amazonka-s3" or (errorHandler.buildDepError "amazonka-s3"))
              (hsPkgs."beam-migrate" or (errorHandler.buildDepError "beam-migrate"))
              (hsPkgs."beam-postgres" or (errorHandler.buildDepError "beam-postgres"))
              (hsPkgs."blaze-markup" or (errorHandler.buildDepError "blaze-markup"))
              (hsPkgs."concat-classes" or (errorHandler.buildDepError "concat-classes"))
              (hsPkgs."concat-examples" or (errorHandler.buildDepError "concat-examples"))
              (hsPkgs."concat-graphics" or (errorHandler.buildDepError "concat-graphics"))
              (hsPkgs."concat-plugin" or (errorHandler.buildDepError "concat-plugin"))
              (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
              (hsPkgs."connection" or (errorHandler.buildDepError "connection"))
              (hsPkgs."dhall" or (errorHandler.buildDepError "dhall"))
              (hsPkgs."fusion-plugin" or (errorHandler.buildDepError "fusion-plugin"))
              (hsPkgs."greskell-websocket" or (errorHandler.buildDepError "greskell-websocket"))
              (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
              (hsPkgs."katip" or (errorHandler.buildDepError "katip"))
              (hsPkgs."net-mqtt" or (errorHandler.buildDepError "net-mqtt"))
              (hsPkgs."net-spider" or (errorHandler.buildDepError "net-spider"))
              (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
              (hsPkgs."postgresql-simple" or (errorHandler.buildDepError "postgresql-simple"))
              (hsPkgs."sbv" or (errorHandler.buildDepError "sbv"))
              (hsPkgs."servant-blaze" or (errorHandler.buildDepError "servant-blaze"))
              (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
              (hsPkgs."servant-js" or (errorHandler.buildDepError "servant-js"))
              (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
              (hsPkgs."servant-websockets" or (errorHandler.buildDepError "servant-websockets"))
              (hsPkgs."suavemente" or (errorHandler.buildDepError "suavemente"))
              (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
              (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
              (hsPkgs."wai-cors" or (errorHandler.buildDepError "wai-cors"))
              (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
              (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
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
          hsSourceDirs = [ "app" ];
          mainPath = ([ "Dev.hs" ] ++ [ "" ]) ++ [ "" ];
          };
        "kbtzim" = {
          depends = ([
            (hsPkgs."SVGFonts" or (errorHandler.buildDepError "SVGFonts"))
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
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64" or (errorHandler.buildDepError "base64"))
            (hsPkgs."beam-core" or (errorHandler.buildDepError "beam-core"))
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
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."ghcjs-dom" or (errorHandler.buildDepError "ghcjs-dom"))
            (hsPkgs."greskell" or (errorHandler.buildDepError "greskell"))
            (hsPkgs."greskell-core" or (errorHandler.buildDepError "greskell-core"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."haxl" or (errorHandler.buildDepError "haxl"))
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
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-client-js" or (errorHandler.buildDepError "servant-client-js"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
            (hsPkgs."unagi-chan" or (errorHandler.buildDepError "unagi-chan"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vector-sized" or (errorHandler.buildDepError "vector-sized"))
            ] ++ (if !(compiler.isGhcjs && true)
            then [
              (hsPkgs."amazonka" or (errorHandler.buildDepError "amazonka"))
              (hsPkgs."amazonka-iot" or (errorHandler.buildDepError "amazonka-iot"))
              (hsPkgs."amazonka-s3" or (errorHandler.buildDepError "amazonka-s3"))
              (hsPkgs."beam-migrate" or (errorHandler.buildDepError "beam-migrate"))
              (hsPkgs."beam-postgres" or (errorHandler.buildDepError "beam-postgres"))
              (hsPkgs."blaze-markup" or (errorHandler.buildDepError "blaze-markup"))
              (hsPkgs."concat-classes" or (errorHandler.buildDepError "concat-classes"))
              (hsPkgs."concat-examples" or (errorHandler.buildDepError "concat-examples"))
              (hsPkgs."concat-graphics" or (errorHandler.buildDepError "concat-graphics"))
              (hsPkgs."concat-plugin" or (errorHandler.buildDepError "concat-plugin"))
              (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
              (hsPkgs."connection" or (errorHandler.buildDepError "connection"))
              (hsPkgs."dhall" or (errorHandler.buildDepError "dhall"))
              (hsPkgs."fusion-plugin" or (errorHandler.buildDepError "fusion-plugin"))
              (hsPkgs."greskell-websocket" or (errorHandler.buildDepError "greskell-websocket"))
              (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
              (hsPkgs."katip" or (errorHandler.buildDepError "katip"))
              (hsPkgs."net-mqtt" or (errorHandler.buildDepError "net-mqtt"))
              (hsPkgs."net-spider" or (errorHandler.buildDepError "net-spider"))
              (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
              (hsPkgs."postgresql-simple" or (errorHandler.buildDepError "postgresql-simple"))
              (hsPkgs."sbv" or (errorHandler.buildDepError "sbv"))
              (hsPkgs."servant-blaze" or (errorHandler.buildDepError "servant-blaze"))
              (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
              (hsPkgs."servant-js" or (errorHandler.buildDepError "servant-js"))
              (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
              (hsPkgs."servant-websockets" or (errorHandler.buildDepError "servant-websockets"))
              (hsPkgs."suavemente" or (errorHandler.buildDepError "suavemente"))
              (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
              (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
              (hsPkgs."wai-cors" or (errorHandler.buildDepError "wai-cors"))
              (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
              (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
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
          mainPath = ([ "Main.hs" ] ++ [ "" ]) ++ [ "" ];
          };
        "server" = {
          depends = ([
            (hsPkgs."SVGFonts" or (errorHandler.buildDepError "SVGFonts"))
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
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64" or (errorHandler.buildDepError "base64"))
            (hsPkgs."beam-core" or (errorHandler.buildDepError "beam-core"))
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
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."ghcjs-dom" or (errorHandler.buildDepError "ghcjs-dom"))
            (hsPkgs."greskell" or (errorHandler.buildDepError "greskell"))
            (hsPkgs."greskell-core" or (errorHandler.buildDepError "greskell-core"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."haxl" or (errorHandler.buildDepError "haxl"))
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
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-client-js" or (errorHandler.buildDepError "servant-client-js"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
            (hsPkgs."unagi-chan" or (errorHandler.buildDepError "unagi-chan"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vector-sized" or (errorHandler.buildDepError "vector-sized"))
            ] ++ (if !(compiler.isGhcjs && true)
            then [
              (hsPkgs."amazonka" or (errorHandler.buildDepError "amazonka"))
              (hsPkgs."amazonka-iot" or (errorHandler.buildDepError "amazonka-iot"))
              (hsPkgs."amazonka-s3" or (errorHandler.buildDepError "amazonka-s3"))
              (hsPkgs."beam-migrate" or (errorHandler.buildDepError "beam-migrate"))
              (hsPkgs."beam-postgres" or (errorHandler.buildDepError "beam-postgres"))
              (hsPkgs."blaze-markup" or (errorHandler.buildDepError "blaze-markup"))
              (hsPkgs."concat-classes" or (errorHandler.buildDepError "concat-classes"))
              (hsPkgs."concat-examples" or (errorHandler.buildDepError "concat-examples"))
              (hsPkgs."concat-graphics" or (errorHandler.buildDepError "concat-graphics"))
              (hsPkgs."concat-plugin" or (errorHandler.buildDepError "concat-plugin"))
              (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
              (hsPkgs."connection" or (errorHandler.buildDepError "connection"))
              (hsPkgs."dhall" or (errorHandler.buildDepError "dhall"))
              (hsPkgs."fusion-plugin" or (errorHandler.buildDepError "fusion-plugin"))
              (hsPkgs."greskell-websocket" or (errorHandler.buildDepError "greskell-websocket"))
              (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
              (hsPkgs."katip" or (errorHandler.buildDepError "katip"))
              (hsPkgs."net-mqtt" or (errorHandler.buildDepError "net-mqtt"))
              (hsPkgs."net-spider" or (errorHandler.buildDepError "net-spider"))
              (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
              (hsPkgs."postgresql-simple" or (errorHandler.buildDepError "postgresql-simple"))
              (hsPkgs."sbv" or (errorHandler.buildDepError "sbv"))
              (hsPkgs."servant-blaze" or (errorHandler.buildDepError "servant-blaze"))
              (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
              (hsPkgs."servant-js" or (errorHandler.buildDepError "servant-js"))
              (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
              (hsPkgs."servant-websockets" or (errorHandler.buildDepError "servant-websockets"))
              (hsPkgs."suavemente" or (errorHandler.buildDepError "suavemente"))
              (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
              (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
              (hsPkgs."wai-cors" or (errorHandler.buildDepError "wai-cors"))
              (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
              (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
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
          mainPath = ([ "Server.hs" ] ++ [ "" ]) ++ [ "" ];
          };
        "ui" = {
          depends = ([
            (hsPkgs."SVGFonts" or (errorHandler.buildDepError "SVGFonts"))
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
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64" or (errorHandler.buildDepError "base64"))
            (hsPkgs."beam-core" or (errorHandler.buildDepError "beam-core"))
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
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."ghcjs-dom" or (errorHandler.buildDepError "ghcjs-dom"))
            (hsPkgs."greskell" or (errorHandler.buildDepError "greskell"))
            (hsPkgs."greskell-core" or (errorHandler.buildDepError "greskell-core"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."haxl" or (errorHandler.buildDepError "haxl"))
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
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-client-js" or (errorHandler.buildDepError "servant-client-js"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
            (hsPkgs."unagi-chan" or (errorHandler.buildDepError "unagi-chan"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vector-sized" or (errorHandler.buildDepError "vector-sized"))
            ] ++ (if !(compiler.isGhcjs && true)
            then [
              (hsPkgs."amazonka" or (errorHandler.buildDepError "amazonka"))
              (hsPkgs."amazonka-iot" or (errorHandler.buildDepError "amazonka-iot"))
              (hsPkgs."amazonka-s3" or (errorHandler.buildDepError "amazonka-s3"))
              (hsPkgs."beam-migrate" or (errorHandler.buildDepError "beam-migrate"))
              (hsPkgs."beam-postgres" or (errorHandler.buildDepError "beam-postgres"))
              (hsPkgs."blaze-markup" or (errorHandler.buildDepError "blaze-markup"))
              (hsPkgs."concat-classes" or (errorHandler.buildDepError "concat-classes"))
              (hsPkgs."concat-examples" or (errorHandler.buildDepError "concat-examples"))
              (hsPkgs."concat-graphics" or (errorHandler.buildDepError "concat-graphics"))
              (hsPkgs."concat-plugin" or (errorHandler.buildDepError "concat-plugin"))
              (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
              (hsPkgs."connection" or (errorHandler.buildDepError "connection"))
              (hsPkgs."dhall" or (errorHandler.buildDepError "dhall"))
              (hsPkgs."fusion-plugin" or (errorHandler.buildDepError "fusion-plugin"))
              (hsPkgs."greskell-websocket" or (errorHandler.buildDepError "greskell-websocket"))
              (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
              (hsPkgs."katip" or (errorHandler.buildDepError "katip"))
              (hsPkgs."net-mqtt" or (errorHandler.buildDepError "net-mqtt"))
              (hsPkgs."net-spider" or (errorHandler.buildDepError "net-spider"))
              (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
              (hsPkgs."postgresql-simple" or (errorHandler.buildDepError "postgresql-simple"))
              (hsPkgs."sbv" or (errorHandler.buildDepError "sbv"))
              (hsPkgs."servant-blaze" or (errorHandler.buildDepError "servant-blaze"))
              (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
              (hsPkgs."servant-js" or (errorHandler.buildDepError "servant-js"))
              (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
              (hsPkgs."servant-websockets" or (errorHandler.buildDepError "servant-websockets"))
              (hsPkgs."suavemente" or (errorHandler.buildDepError "suavemente"))
              (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
              (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
              (hsPkgs."wai-cors" or (errorHandler.buildDepError "wai-cors"))
              (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
              (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
              (hsPkgs."x509-store" or (errorHandler.buildDepError "x509-store"))
              (hsPkgs."x509-validation" or (errorHandler.buildDepError "x509-validation"))
              ]
            else [
              (hsPkgs."ghcjs-base" or (errorHandler.buildDepError "ghcjs-base"))
              ])) ++ [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."chopaan" or (errorHandler.buildDepError "chopaan"))
            (hsPkgs."optparse-simple" or (errorHandler.buildDepError "optparse-simple"))
            ];
          buildable = true;
          hsSourceDirs = [ "app" ];
          mainPath = ([ "Ui.hs" ] ++ [ "" ]) ++ [ "" ];
          };
        };
      tests = {
        "chopaan-test" = {
          depends = ([
            (hsPkgs."SVGFonts" or (errorHandler.buildDepError "SVGFonts"))
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
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64" or (errorHandler.buildDepError "base64"))
            (hsPkgs."beam-core" or (errorHandler.buildDepError "beam-core"))
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
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."ghcjs-dom" or (errorHandler.buildDepError "ghcjs-dom"))
            (hsPkgs."greskell" or (errorHandler.buildDepError "greskell"))
            (hsPkgs."greskell-core" or (errorHandler.buildDepError "greskell-core"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."haxl" or (errorHandler.buildDepError "haxl"))
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
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-client-js" or (errorHandler.buildDepError "servant-client-js"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."time-compat" or (errorHandler.buildDepError "time-compat"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
            (hsPkgs."unagi-chan" or (errorHandler.buildDepError "unagi-chan"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vector-sized" or (errorHandler.buildDepError "vector-sized"))
            ] ++ (if !(compiler.isGhcjs && true)
            then [
              (hsPkgs."amazonka" or (errorHandler.buildDepError "amazonka"))
              (hsPkgs."amazonka-iot" or (errorHandler.buildDepError "amazonka-iot"))
              (hsPkgs."amazonka-s3" or (errorHandler.buildDepError "amazonka-s3"))
              (hsPkgs."beam-migrate" or (errorHandler.buildDepError "beam-migrate"))
              (hsPkgs."beam-postgres" or (errorHandler.buildDepError "beam-postgres"))
              (hsPkgs."blaze-markup" or (errorHandler.buildDepError "blaze-markup"))
              (hsPkgs."concat-classes" or (errorHandler.buildDepError "concat-classes"))
              (hsPkgs."concat-examples" or (errorHandler.buildDepError "concat-examples"))
              (hsPkgs."concat-graphics" or (errorHandler.buildDepError "concat-graphics"))
              (hsPkgs."concat-plugin" or (errorHandler.buildDepError "concat-plugin"))
              (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
              (hsPkgs."connection" or (errorHandler.buildDepError "connection"))
              (hsPkgs."dhall" or (errorHandler.buildDepError "dhall"))
              (hsPkgs."fusion-plugin" or (errorHandler.buildDepError "fusion-plugin"))
              (hsPkgs."greskell-websocket" or (errorHandler.buildDepError "greskell-websocket"))
              (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
              (hsPkgs."katip" or (errorHandler.buildDepError "katip"))
              (hsPkgs."net-mqtt" or (errorHandler.buildDepError "net-mqtt"))
              (hsPkgs."net-spider" or (errorHandler.buildDepError "net-spider"))
              (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
              (hsPkgs."postgresql-simple" or (errorHandler.buildDepError "postgresql-simple"))
              (hsPkgs."sbv" or (errorHandler.buildDepError "sbv"))
              (hsPkgs."servant-blaze" or (errorHandler.buildDepError "servant-blaze"))
              (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
              (hsPkgs."servant-js" or (errorHandler.buildDepError "servant-js"))
              (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
              (hsPkgs."servant-websockets" or (errorHandler.buildDepError "servant-websockets"))
              (hsPkgs."suavemente" or (errorHandler.buildDepError "suavemente"))
              (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
              (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
              (hsPkgs."wai-cors" or (errorHandler.buildDepError "wai-cors"))
              (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
              (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
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