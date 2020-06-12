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
      specVersion = "1.10";
      identifier = { name = "concat-hardware"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "2017 Conal Elliott, David Banas";
      maintainer = "capn.freako@gmail.com";
      author = "Conal Elliott, David Banas";
      homepage = "";
      url = "";
      synopsis = "Circuit description (HDL) via compiling-to-categories";
      description = "Circuit description (HDL) via compiling-to-categories";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."concat-classes" or (errorHandler.buildDepError "concat-classes"))
          (hsPkgs."concat-examples" or (errorHandler.buildDepError "concat-examples"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."prettyclass" or (errorHandler.buildDepError "prettyclass"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."verilog" or (errorHandler.buildDepError "verilog"))
          (hsPkgs."netlist" or (errorHandler.buildDepError "netlist"))
          (hsPkgs."netlist-to-verilog" or (errorHandler.buildDepError "netlist-to-verilog"))
          ];
        buildable = true;
        };
      tests = {
        "hardware-examples" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."concat-classes" or (errorHandler.buildDepError "concat-classes"))
            (hsPkgs."concat-examples" or (errorHandler.buildDepError "concat-examples"))
            (hsPkgs."concat-hardware" or (errorHandler.buildDepError "concat-hardware"))
            (hsPkgs."concat-plugin" or (errorHandler.buildDepError "concat-plugin"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."verilog" or (errorHandler.buildDepError "verilog"))
            (hsPkgs."netlist" or (errorHandler.buildDepError "netlist"))
            (hsPkgs."netlist-to-verilog" or (errorHandler.buildDepError "netlist-to-verilog"))
            ];
          buildable = true;
          };
        "hardware-trace" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."concat-classes" or (errorHandler.buildDepError "concat-classes"))
            (hsPkgs."concat-examples" or (errorHandler.buildDepError "concat-examples"))
            (hsPkgs."concat-hardware" or (errorHandler.buildDepError "concat-hardware"))
            (hsPkgs."concat-plugin" or (errorHandler.buildDepError "concat-plugin"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."verilog" or (errorHandler.buildDepError "verilog"))
            (hsPkgs."netlist" or (errorHandler.buildDepError "netlist"))
            (hsPkgs."netlist-to-verilog" or (errorHandler.buildDepError "netlist-to-verilog"))
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
    postUnpack = "sourceRoot+=/hardware; echo source root reset to \$sourceRoot";
    }