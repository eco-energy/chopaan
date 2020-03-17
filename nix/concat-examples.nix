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
  {
    flags = { smt = false; };
    package = {
      specVersion = "1.18";
      identifier = { name = "concat-examples"; version = "0.3.0.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2016-2017 by Conal Elliott";
      maintainer = "conal@conal.net";
      author = "Conal Elliott";
      homepage = "";
      url = "";
      synopsis = "Some examples of compiling to categories";
      description = "Some examples of compiling to categories";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."newtype-generics" or (buildDepError "newtype-generics"))
          (hsPkgs."constraints" or (buildDepError "constraints"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."keys" or (buildDepError "keys"))
          (hsPkgs."pointed" or (buildDepError "pointed"))
          (hsPkgs."distributive" or (buildDepError "distributive"))
          (hsPkgs."adjunctions" or (buildDepError "adjunctions"))
          (hsPkgs."prettyclass" or (buildDepError "prettyclass"))
          (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."process" or (buildDepError "process"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."finite-typelits" or (buildDepError "finite-typelits"))
          (hsPkgs."ghc-typelits-knownnat" or (buildDepError "ghc-typelits-knownnat"))
          (hsPkgs."ghc-typelits-natnormalise" or (buildDepError "ghc-typelits-natnormalise"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."data-default" or (buildDepError "data-default"))
          (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
          (hsPkgs."vector" or (buildDepError "vector"))
          (hsPkgs."vector-sized" or (buildDepError "vector-sized"))
          (hsPkgs."NumInstances" or (buildDepError "NumInstances"))
          (hsPkgs."concat-inline" or (buildDepError "concat-inline"))
          (hsPkgs."concat-known" or (buildDepError "concat-known"))
          (hsPkgs."concat-classes" or (buildDepError "concat-classes"))
          (hsPkgs."concat-plugin" or (buildDepError "concat-plugin"))
          ] ++ (pkgs.lib).optional (flags.smt) (hsPkgs."z3" or (buildDepError "z3"));
        buildable = true;
        };
      tests = {
        "misc-examples" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."Cabal" or (buildDepError "Cabal"))
            (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
            (hsPkgs."constraints" or (buildDepError "constraints"))
            (hsPkgs."newtype-generics" or (buildDepError "newtype-generics"))
            (hsPkgs."pointed" or (buildDepError "pointed"))
            (hsPkgs."keys" or (buildDepError "keys"))
            (hsPkgs."distributive" or (buildDepError "distributive"))
            (hsPkgs."adjunctions" or (buildDepError "adjunctions"))
            (hsPkgs."concat-inline" or (buildDepError "concat-inline"))
            (hsPkgs."concat-classes" or (buildDepError "concat-classes"))
            (hsPkgs."concat-plugin" or (buildDepError "concat-plugin"))
            (hsPkgs."concat-examples" or (buildDepError "concat-examples"))
            (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
            (hsPkgs."integer-gmp" or (buildDepError "integer-gmp"))
            (hsPkgs."distributive" or (buildDepError "distributive"))
            (hsPkgs."adjunctions" or (buildDepError "adjunctions"))
            (hsPkgs."constraints" or (buildDepError "constraints"))
            (hsPkgs."finite-typelits" or (buildDepError "finite-typelits"))
            (hsPkgs."vector-sized" or (buildDepError "vector-sized"))
            ];
          buildable = true;
          };
        "misc-trace" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."Cabal" or (buildDepError "Cabal"))
            (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
            (hsPkgs."constraints" or (buildDepError "constraints"))
            (hsPkgs."newtype-generics" or (buildDepError "newtype-generics"))
            (hsPkgs."pointed" or (buildDepError "pointed"))
            (hsPkgs."keys" or (buildDepError "keys"))
            (hsPkgs."distributive" or (buildDepError "distributive"))
            (hsPkgs."adjunctions" or (buildDepError "adjunctions"))
            (hsPkgs."concat-inline" or (buildDepError "concat-inline"))
            (hsPkgs."concat-classes" or (buildDepError "concat-classes"))
            (hsPkgs."concat-plugin" or (buildDepError "concat-plugin"))
            (hsPkgs."concat-examples" or (buildDepError "concat-examples"))
            (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
            (hsPkgs."integer-gmp" or (buildDepError "integer-gmp"))
            (hsPkgs."keys" or (buildDepError "keys"))
            (hsPkgs."distributive" or (buildDepError "distributive"))
            (hsPkgs."adjunctions" or (buildDepError "adjunctions"))
            (hsPkgs."constraints" or (buildDepError "constraints"))
            (hsPkgs."finite-typelits" or (buildDepError "finite-typelits"))
            (hsPkgs."vector-sized" or (buildDepError "vector-sized"))
            ];
          buildable = true;
          };
        "gold-tests" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."Cabal" or (buildDepError "Cabal"))
            (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
            (hsPkgs."constraints" or (buildDepError "constraints"))
            (hsPkgs."newtype-generics" or (buildDepError "newtype-generics"))
            (hsPkgs."pointed" or (buildDepError "pointed"))
            (hsPkgs."keys" or (buildDepError "keys"))
            (hsPkgs."distributive" or (buildDepError "distributive"))
            (hsPkgs."adjunctions" or (buildDepError "adjunctions"))
            (hsPkgs."concat-inline" or (buildDepError "concat-inline"))
            (hsPkgs."concat-classes" or (buildDepError "concat-classes"))
            (hsPkgs."concat-plugin" or (buildDepError "concat-plugin"))
            (hsPkgs."concat-examples" or (buildDepError "concat-examples"))
            (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
            (hsPkgs."integer-gmp" or (buildDepError "integer-gmp"))
            (hsPkgs."distributive" or (buildDepError "distributive"))
            (hsPkgs."adjunctions" or (buildDepError "adjunctions"))
            (hsPkgs."constraints" or (buildDepError "constraints"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (buildDepError "tasty-golden"))
            (hsPkgs."finite-typelits" or (buildDepError "finite-typelits"))
            (hsPkgs."vector-sized" or (buildDepError "vector-sized"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/conal/concat.git";
      rev = "6da06f44947ea8909fa3d60e94a238f7bc998bb6";
      sha256 = "1a3z21n0ispcc4irggkwsclfbv87z1gb52lfafwx09n8bszr8c19";
      });
    postUnpack = "sourceRoot+=/examples; echo source root reset to \$sourceRoot";
    }