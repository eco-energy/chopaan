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
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "concat-graphics"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "2017 Conal Elliott";
      maintainer = "conal@conal.net";
      author = "Conal Elliott";
      homepage = "";
      url = "";
      synopsis = "Graphics via compiling-to-categories";
      description = "Graphics via compiling-to-categories";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."aeson-pretty" or (buildDepError "aeson-pretty"))
          (hsPkgs."NumInstances" or (buildDepError "NumInstances"))
          (hsPkgs."concat-classes" or (buildDepError "concat-classes"))
          (hsPkgs."concat-examples" or (buildDepError "concat-examples"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."language-glsl" or (buildDepError "language-glsl"))
          (hsPkgs."parsec" or (buildDepError "parsec"))
          (hsPkgs."prettyclass" or (buildDepError "prettyclass"))
          (hsPkgs."text" or (buildDepError "text"))
          ];
        buildable = true;
        };
      tests = {
        "graphics-examples" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (buildDepError "aeson-pretty"))
            (hsPkgs."concat-classes" or (buildDepError "concat-classes"))
            (hsPkgs."concat-examples" or (buildDepError "concat-examples"))
            (hsPkgs."concat-graphics" or (buildDepError "concat-graphics"))
            (hsPkgs."concat-plugin" or (buildDepError "concat-plugin"))
            (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
            (hsPkgs."language-glsl" or (buildDepError "language-glsl"))
            (hsPkgs."newtype-generics" or (buildDepError "newtype-generics"))
            (hsPkgs."text" or (buildDepError "text"))
            ];
          buildable = true;
          };
        "graphics-trace" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."aeson-pretty" or (buildDepError "aeson-pretty"))
            (hsPkgs."concat-classes" or (buildDepError "concat-classes"))
            (hsPkgs."concat-examples" or (buildDepError "concat-examples"))
            (hsPkgs."concat-graphics" or (buildDepError "concat-graphics"))
            (hsPkgs."concat-plugin" or (buildDepError "concat-plugin"))
            (hsPkgs."ghc-prim" or (buildDepError "ghc-prim"))
            (hsPkgs."language-glsl" or (buildDepError "language-glsl"))
            (hsPkgs."newtype-generics" or (buildDepError "newtype-generics"))
            (hsPkgs."text" or (buildDepError "text"))
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
    postUnpack = "sourceRoot+=/graphics; echo source root reset to \$sourceRoot";
    }