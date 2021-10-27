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
      identifier = { name = "ghcjs-three"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "2015 Eric Wong";
      maintainer = "ericsyw@gmail.com";
      author = "Eric Wong";
      homepage = "http://github.com/manyoo/ghcjs-three#readme";
      url = "";
      synopsis = "A Three.js wrapper for GHCJS";
      description = "Please see README.md";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
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
          (hsPkgs."ghcjs-dom" or (errorHandler.buildDepError "ghcjs-dom"))
          (hsPkgs."jsaddle" or (errorHandler.buildDepError "jsaddle"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."linear" or (errorHandler.buildDepError "linear"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          ] ++ (pkgs.lib).optional (compiler.isGhcjs && true) (hsPkgs."ghcjs-base" or (errorHandler.buildDepError "ghcjs-base"));
        buildable = true;
        modules = [
          "GHCJS/Three/Monad"
          "GHCJS/Three/Matrix"
          "GHCJS/Three/Vector"
          "GHCJS/Three/Face3"
          "GHCJS/Three/Box3"
          "GHCJS/Three/Sphere"
          "GHCJS/Three/Object3D"
          "GHCJS/Three/Camera"
          "GHCJS/Three/Projection"
          "GHCJS/Three/Color"
          "GHCJS/Three/HasName"
          "GHCJS/Three/Disposable"
          "GHCJS/Three/HasXYZ"
          "GHCJS/Three/HasGeoMat"
          "GHCJS/Three/CanCopy"
          "GHCJS/Three/Euler"
          "GHCJS/Three/Geometry"
          "GHCJS/Three/GLNode"
          "GHCJS/Three/Light"
          "GHCJS/Three/Material"
          "GHCJS/Three/Visible"
          "GHCJS/Three/Mesh"
          "GHCJS/Three/Line"
          "GHCJS/Three/Quaternion"
          "GHCJS/Three/Ray"
          "GHCJS/Three/Raycaster"
          "GHCJS/Three/Scene"
          "GHCJS/Three/WebGLRenderer"
          "GHCJS/Three/Texture"
          "GHCJS/Three/Path"
          "GHCJS/Three/Shape"
          "GHCJS/Three/ShapeGeometry"
          "GHCJS/Three/CylinderGeometry"
          "GHCJS/Three/BufferGeometry"
          "GHCJS/Three/CameraHelper"
          "GHCJS/Three/MTLLoader"
          "GHCJS/Three/OBJLoader"
          "GHCJS/Three/Font"
          "GHCJS/Three/FontLoader"
          "GHCJS/Three/TextGeometry"
          "GHCJS/Three"
          ];
        jsSources = [ "three/three.min.js" ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "ghcjs-three-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ghcjs-three" or (errorHandler.buildDepError "ghcjs-three"))
            ];
          buildable = true;
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././.source-repository-packages/7; }