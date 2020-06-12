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
      specVersion = "2.0";
      identifier = { name = "chopaan"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "2019 Faez Shakil";
      maintainer = "faez.shakil@gmail.com";
      author = "Faez Shakil";
      homepage = "https://github.com/faezs/chopaan#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on Github at <https://github.com/faezs/chopaan#readme>";
      buildType = "Custom";
      isLocal = true;
      setup-depends = [
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (errorHandler.buildToolDepError "Cabal")))
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (errorHandler.buildToolDepError "base")))
        (hsPkgs.buildPackages.proto-lens-setup or (pkgs.buildPackages.proto-lens-setup or (errorHandler.buildToolDepError "proto-lens-setup")))
        ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."ad" or (errorHandler.buildDepError "ad"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."amazonka" or (errorHandler.buildDepError "amazonka"))
          (hsPkgs."amazonka-iot" or (errorHandler.buildDepError "amazonka-iot"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
          (hsPkgs."compensated" or (errorHandler.buildDepError "compensated"))
          (hsPkgs."concat-classes" or (errorHandler.buildDepError "concat-classes"))
          (hsPkgs."connection" or (errorHandler.buildDepError "connection"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."convertible" or (errorHandler.buildDepError "convertible"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."dhall" or (errorHandler.buildDepError "dhall"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
          (hsPkgs."estimator" or (errorHandler.buildDepError "estimator"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hmatrix" or (errorHandler.buildDepError "hmatrix"))
          (hsPkgs."kalman" or (errorHandler.buildDepError "kalman"))
          (hsPkgs."keys" or (errorHandler.buildDepError "keys"))
          (hsPkgs."linear" or (errorHandler.buildDepError "linear"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."net-mqtt" or (errorHandler.buildDepError "net-mqtt"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."numeric-limits" or (errorHandler.buildDepError "numeric-limits"))
          (hsPkgs."proto-lens" or (errorHandler.buildDepError "proto-lens"))
          (hsPkgs."proto-lens-runtime" or (errorHandler.buildDepError "proto-lens-runtime"))
          (hsPkgs."rio" or (errorHandler.buildDepError "rio"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."stm-containers" or (errorHandler.buildDepError "stm-containers"))
          (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
          (hsPkgs."streamly-bytestring" or (errorHandler.buildDepError "streamly-bytestring"))
          (hsPkgs."streamly-cassava" or (errorHandler.buildDepError "streamly-cassava"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."time-lens" or (errorHandler.buildDepError "time-lens"))
          (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
          (hsPkgs."total-map" or (errorHandler.buildDepError "total-map"))
          (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
          (hsPkgs."x509-store" or (errorHandler.buildDepError "x509-store"))
          (hsPkgs."x509-validation" or (errorHandler.buildDepError "x509-validation"))
          ];
        build-tools = [
          (hsPkgs.buildPackages.proto-lens-protoc or (pkgs.buildPackages.proto-lens-protoc or (errorHandler.buildToolDepError "proto-lens-protoc")))
          ];
        buildable = true;
        };
      exes = {
        "chopaan-exe" = {
          depends = [
            (hsPkgs."ad" or (errorHandler.buildDepError "ad"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."amazonka" or (errorHandler.buildDepError "amazonka"))
            (hsPkgs."amazonka-iot" or (errorHandler.buildDepError "amazonka-iot"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."chopaan" or (errorHandler.buildDepError "chopaan"))
            (hsPkgs."compensated" or (errorHandler.buildDepError "compensated"))
            (hsPkgs."concat-classes" or (errorHandler.buildDepError "concat-classes"))
            (hsPkgs."connection" or (errorHandler.buildDepError "connection"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."convertible" or (errorHandler.buildDepError "convertible"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."dhall" or (errorHandler.buildDepError "dhall"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
            (hsPkgs."estimator" or (errorHandler.buildDepError "estimator"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."hmatrix" or (errorHandler.buildDepError "hmatrix"))
            (hsPkgs."kalman" or (errorHandler.buildDepError "kalman"))
            (hsPkgs."keys" or (errorHandler.buildDepError "keys"))
            (hsPkgs."linear" or (errorHandler.buildDepError "linear"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."net-mqtt" or (errorHandler.buildDepError "net-mqtt"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."numeric-limits" or (errorHandler.buildDepError "numeric-limits"))
            (hsPkgs."optparse-simple" or (errorHandler.buildDepError "optparse-simple"))
            (hsPkgs."proto-lens" or (errorHandler.buildDepError "proto-lens"))
            (hsPkgs."proto-lens-runtime" or (errorHandler.buildDepError "proto-lens-runtime"))
            (hsPkgs."rio" or (errorHandler.buildDepError "rio"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."stm-containers" or (errorHandler.buildDepError "stm-containers"))
            (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
            (hsPkgs."streamly-bytestring" or (errorHandler.buildDepError "streamly-bytestring"))
            (hsPkgs."streamly-cassava" or (errorHandler.buildDepError "streamly-cassava"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."time-lens" or (errorHandler.buildDepError "time-lens"))
            (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
            (hsPkgs."total-map" or (errorHandler.buildDepError "total-map"))
            (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."x509-store" or (errorHandler.buildDepError "x509-store"))
            (hsPkgs."x509-validation" or (errorHandler.buildDepError "x509-validation"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.proto-lens-protoc or (pkgs.buildPackages.proto-lens-protoc or (errorHandler.buildToolDepError "proto-lens-protoc")))
            ];
          buildable = true;
          };
        };
      tests = {
        "chopaan-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."ad" or (errorHandler.buildDepError "ad"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."amazonka" or (errorHandler.buildDepError "amazonka"))
            (hsPkgs."amazonka-iot" or (errorHandler.buildDepError "amazonka-iot"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."checkers" or (errorHandler.buildDepError "checkers"))
            (hsPkgs."chopaan" or (errorHandler.buildDepError "chopaan"))
            (hsPkgs."compensated" or (errorHandler.buildDepError "compensated"))
            (hsPkgs."concat-classes" or (errorHandler.buildDepError "concat-classes"))
            (hsPkgs."connection" or (errorHandler.buildDepError "connection"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."convertible" or (errorHandler.buildDepError "convertible"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."dhall" or (errorHandler.buildDepError "dhall"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
            (hsPkgs."estimator" or (errorHandler.buildDepError "estimator"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."hmatrix" or (errorHandler.buildDepError "hmatrix"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."kalman" or (errorHandler.buildDepError "kalman"))
            (hsPkgs."keys" or (errorHandler.buildDepError "keys"))
            (hsPkgs."linear" or (errorHandler.buildDepError "linear"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."net-mqtt" or (errorHandler.buildDepError "net-mqtt"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."numeric-limits" or (errorHandler.buildDepError "numeric-limits"))
            (hsPkgs."proto-lens" or (errorHandler.buildDepError "proto-lens"))
            (hsPkgs."proto-lens-arbitrary" or (errorHandler.buildDepError "proto-lens-arbitrary"))
            (hsPkgs."proto-lens-runtime" or (errorHandler.buildDepError "proto-lens-runtime"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."rio" or (errorHandler.buildDepError "rio"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."stm-containers" or (errorHandler.buildDepError "stm-containers"))
            (hsPkgs."streamly" or (errorHandler.buildDepError "streamly"))
            (hsPkgs."streamly-bytestring" or (errorHandler.buildDepError "streamly-bytestring"))
            (hsPkgs."streamly-cassava" or (errorHandler.buildDepError "streamly-cassava"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."time-lens" or (errorHandler.buildDepError "time-lens"))
            (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
            (hsPkgs."total-map" or (errorHandler.buildDepError "total-map"))
            (hsPkgs."ulid" or (errorHandler.buildDepError "ulid"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."x509-store" or (errorHandler.buildDepError "x509-store"))
            (hsPkgs."x509-validation" or (errorHandler.buildDepError "x509-validation"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.proto-lens-protoc or (pkgs.buildPackages.proto-lens-protoc or (errorHandler.buildToolDepError "proto-lens-protoc")))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././.; }) // {
    cabal-generator = "hpack";
    }