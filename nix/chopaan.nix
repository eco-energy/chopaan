let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).
      
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).
      
      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
      '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
      '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).
      
      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      
      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  ({
    flags = {};
    package = {
      specVersion = "0";
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
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (buildToolDepError "Cabal")))
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (buildToolDepError "base")))
        (hsPkgs.buildPackages.proto-lens-setup or (pkgs.buildPackages.proto-lens-setup or (buildToolDepError "proto-lens-setup")))
        ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."amazonka" or (buildDepError "amazonka"))
          (hsPkgs."amazonka-iot" or (buildDepError "amazonka-iot"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."brick" or (buildDepError "brick"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."checkers" or (buildDepError "checkers"))
          (hsPkgs."concat-classes" or (buildDepError "concat-classes"))
          (hsPkgs."concat-examples" or (buildDepError "concat-examples"))
          (hsPkgs."connection" or (buildDepError "connection"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."convertible" or (buildDepError "convertible"))
          (hsPkgs."cursor" or (buildDepError "cursor"))
          (hsPkgs."data-default-class" or (buildDepError "data-default-class"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."estimator" or (buildDepError "estimator"))
          (hsPkgs."hashable" or (buildDepError "hashable"))
          (hsPkgs."hmatrix" or (buildDepError "hmatrix"))
          (hsPkgs."hspec" or (buildDepError "hspec"))
          (hsPkgs."kalman" or (buildDepError "kalman"))
          (hsPkgs."microlens" or (buildDepError "microlens"))
          (hsPkgs."microlens-th" or (buildDepError "microlens-th"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."net-mqtt" or (buildDepError "net-mqtt"))
          (hsPkgs."network-uri" or (buildDepError "network-uri"))
          (hsPkgs."proto-lens" or (buildDepError "proto-lens"))
          (hsPkgs."proto-lens-arbitrary" or (buildDepError "proto-lens-arbitrary"))
          (hsPkgs."proto-lens-runtime" or (buildDepError "proto-lens-runtime"))
          (hsPkgs."quickcheck-instances" or (buildDepError "quickcheck-instances"))
          (hsPkgs."reflection" or (buildDepError "reflection"))
          (hsPkgs."rio" or (buildDepError "rio"))
          (hsPkgs."stm" or (buildDepError "stm"))
          (hsPkgs."stm-containers" or (buildDepError "stm-containers"))
          (hsPkgs."streamly" or (buildDepError "streamly"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."time-lens" or (buildDepError "time-lens"))
          (hsPkgs."tls" or (buildDepError "tls"))
          (hsPkgs."ulid" or (buildDepError "ulid"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          (hsPkgs."vector" or (buildDepError "vector"))
          (hsPkgs."vty" or (buildDepError "vty"))
          (hsPkgs."x509-store" or (buildDepError "x509-store"))
          (hsPkgs."x509-validation" or (buildDepError "x509-validation"))
          ];
        build-tools = [
          (hsPkgs.buildPackages.proto-lens-protoc or (pkgs.buildPackages.proto-lens-protoc or (buildToolDepError "proto-lens-protoc")))
          ];
        buildable = true;
        };
      exes = {
        "chopaan-exe" = {
          depends = [
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."amazonka" or (buildDepError "amazonka"))
            (hsPkgs."amazonka-iot" or (buildDepError "amazonka-iot"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."brick" or (buildDepError "brick"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."checkers" or (buildDepError "checkers"))
            (hsPkgs."chopaan" or (buildDepError "chopaan"))
            (hsPkgs."concat-classes" or (buildDepError "concat-classes"))
            (hsPkgs."concat-examples" or (buildDepError "concat-examples"))
            (hsPkgs."connection" or (buildDepError "connection"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."convertible" or (buildDepError "convertible"))
            (hsPkgs."cursor" or (buildDepError "cursor"))
            (hsPkgs."data-default-class" or (buildDepError "data-default-class"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."estimator" or (buildDepError "estimator"))
            (hsPkgs."hashable" or (buildDepError "hashable"))
            (hsPkgs."hmatrix" or (buildDepError "hmatrix"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."kalman" or (buildDepError "kalman"))
            (hsPkgs."microlens" or (buildDepError "microlens"))
            (hsPkgs."microlens-th" or (buildDepError "microlens-th"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."net-mqtt" or (buildDepError "net-mqtt"))
            (hsPkgs."network-uri" or (buildDepError "network-uri"))
            (hsPkgs."optparse-simple" or (buildDepError "optparse-simple"))
            (hsPkgs."proto-lens" or (buildDepError "proto-lens"))
            (hsPkgs."proto-lens-arbitrary" or (buildDepError "proto-lens-arbitrary"))
            (hsPkgs."proto-lens-runtime" or (buildDepError "proto-lens-runtime"))
            (hsPkgs."quickcheck-instances" or (buildDepError "quickcheck-instances"))
            (hsPkgs."reflection" or (buildDepError "reflection"))
            (hsPkgs."rio" or (buildDepError "rio"))
            (hsPkgs."stm" or (buildDepError "stm"))
            (hsPkgs."stm-containers" or (buildDepError "stm-containers"))
            (hsPkgs."streamly" or (buildDepError "streamly"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."time-lens" or (buildDepError "time-lens"))
            (hsPkgs."tls" or (buildDepError "tls"))
            (hsPkgs."ulid" or (buildDepError "ulid"))
            (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
            (hsPkgs."vector" or (buildDepError "vector"))
            (hsPkgs."vty" or (buildDepError "vty"))
            (hsPkgs."x509-store" or (buildDepError "x509-store"))
            (hsPkgs."x509-validation" or (buildDepError "x509-validation"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.proto-lens-protoc or (pkgs.buildPackages.proto-lens-protoc or (buildToolDepError "proto-lens-protoc")))
            ];
          buildable = true;
          };
        };
      tests = {
        "chopaan-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."amazonka" or (buildDepError "amazonka"))
            (hsPkgs."amazonka-iot" or (buildDepError "amazonka-iot"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."brick" or (buildDepError "brick"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."checkers" or (buildDepError "checkers"))
            (hsPkgs."chopaan" or (buildDepError "chopaan"))
            (hsPkgs."concat-classes" or (buildDepError "concat-classes"))
            (hsPkgs."concat-examples" or (buildDepError "concat-examples"))
            (hsPkgs."connection" or (buildDepError "connection"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."convertible" or (buildDepError "convertible"))
            (hsPkgs."cursor" or (buildDepError "cursor"))
            (hsPkgs."data-default-class" or (buildDepError "data-default-class"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."estimator" or (buildDepError "estimator"))
            (hsPkgs."hashable" or (buildDepError "hashable"))
            (hsPkgs."hmatrix" or (buildDepError "hmatrix"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."kalman" or (buildDepError "kalman"))
            (hsPkgs."microlens" or (buildDepError "microlens"))
            (hsPkgs."microlens-th" or (buildDepError "microlens-th"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."net-mqtt" or (buildDepError "net-mqtt"))
            (hsPkgs."network-uri" or (buildDepError "network-uri"))
            (hsPkgs."proto-lens" or (buildDepError "proto-lens"))
            (hsPkgs."proto-lens-arbitrary" or (buildDepError "proto-lens-arbitrary"))
            (hsPkgs."proto-lens-runtime" or (buildDepError "proto-lens-runtime"))
            (hsPkgs."quickcheck-instances" or (buildDepError "quickcheck-instances"))
            (hsPkgs."reflection" or (buildDepError "reflection"))
            (hsPkgs."rio" or (buildDepError "rio"))
            (hsPkgs."stm" or (buildDepError "stm"))
            (hsPkgs."stm-containers" or (buildDepError "stm-containers"))
            (hsPkgs."streamly" or (buildDepError "streamly"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."time-lens" or (buildDepError "time-lens"))
            (hsPkgs."tls" or (buildDepError "tls"))
            (hsPkgs."ulid" or (buildDepError "ulid"))
            (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
            (hsPkgs."vector" or (buildDepError "vector"))
            (hsPkgs."vty" or (buildDepError "vty"))
            (hsPkgs."x509-store" or (buildDepError "x509-store"))
            (hsPkgs."x509-validation" or (buildDepError "x509-validation"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.proto-lens-protoc or (pkgs.buildPackages.proto-lens-protoc or (buildToolDepError "proto-lens-protoc")))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }) // {
    cabal-generator = "hpack";
    }