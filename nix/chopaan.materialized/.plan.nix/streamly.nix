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
    flags = {
      fusion-plugin = false;
      inspection = false;
      debug = false;
      dev = false;
      has-llvm = false;
      no-fusion = false;
      streamk = false;
      use-c-malloc = false;
      opt = true;
      };
    package = {
      specVersion = "2.2";
      identifier = { name = "streamly"; version = "0.8.0"; };
      license = "BSD-3-Clause";
      copyright = "2017 Composewell Technologies";
      maintainer = "streamly@composewell.com";
      author = "Composewell Technologies";
      homepage = "https://streamly.composewell.com";
      url = "";
      synopsis = "Dataflow programming and declarative concurrency";
      description = "Browse the documentation at https://streamly.composewell.com.\n\nStreamly is a streaming framework to build reliable and scalable\nsoftware systems from modular building blocks using dataflow\nprogramming and declarative concurrency.  Stream fusion optimizations\nin streamly result in high-performance, modular combinatorial\nprogramming.\n\nPerformance with simplicity:\n\n* Performance on par with C (<https://github.com/composewell/streaming-benchmarks Benchmarks>)\n* API close to standard Haskell lists (<https://github.com/composewell/streamly-examples Examples>)\n* Declarative concurrency with automatic scaling\n* Filesystem, fsnotify, network, and Unicode support included\n* More functionality provided via many ecosystem packages\n\nUnified and powerful abstractions:\n\n* Unifies unfolds, arrays, folds, and parsers with streaming\n* Unifies @Data.List@, @list-t@, and @logict@ with streaming\n* Unifies concurrency with standard streaming abstractions\n* Provides time-domain combinators for reactive programming\n* Interworks with bytestring and streaming libraries";
      buildType = "Configure";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        ".circleci/config.yml"
        ".ghci"
        ".github/workflows/haskell.yml"
        ".gitignore"
        ".hlint.ignore"
        ".hlint.yaml"
        "CONTRIBUTING.md"
        "Changelog.md"
        "README.md"
        "appveyor.yml"
        "benchmark/*.hs"
        "benchmark/README.md"
        "benchmark/bench-report/BenchReport.hs"
        "benchmark/bench-report/bench-report.cabal"
        "benchmark/bench-report/cabal.project"
        "benchmark/bench-report/default.nix"
        "benchmark/Streamly/Benchmark/Data/*.hs"
        "benchmark/Streamly/Benchmark/Data/Array/Stream/Foreign.hs"
        "benchmark/Streamly/Benchmark/Data/Parser/*.hs"
        "benchmark/Streamly/Benchmark/Data/Stream/*.hs"
        "benchmark/Streamly/Benchmark/FileSystem/*.hs"
        "benchmark/Streamly/Benchmark/FileSystem/Handle/*.hs"
        "benchmark/Streamly/Benchmark/Prelude/*.hs"
        "benchmark/Streamly/Benchmark/Prelude/Serial/*.hs"
        "benchmark/Streamly/Benchmark/Unicode/*.hs"
        "benchmark/lib/Streamly/Benchmark/*.hs"
        "benchmark/lib/Streamly/Benchmark/Common/*.hs"
        "benchmark/streamly-benchmarks.cabal"
        "bin/bench.sh"
        "bin/bench-exec-one.sh"
        "bin/build-lib.sh"
        "bin/ghc.sh"
        "bin/run-ci.sh"
        "bin/mk-hscope.sh"
        "bin/mk-tags.sh"
        "bin/targets.sh"
        "bin/test.sh"
        "configure"
        "configure.ac"
        "credits/*.md"
        "credits/Yampa-0.10.6.2.txt"
        "credits/base-4.12.0.0.txt"
        "credits/bjoern-2008-2009.txt"
        "credits/clock-0.7.2.txt"
        "credits/foldl-1.4.5.txt"
        "credits/fsnotify-0.3.0.1.txt"
        "credits/hfsevents-0.1.6.txt"
        "credits/pipes-concurrency-2.0.8.txt"
        "credits/primitive-0.7.0.0.txt"
        "credits/transient-0.5.5.txt"
        "credits/vector-0.12.0.2.txt"
        "default.nix"
        "dev/*.md"
        "dev/*.png"
        "dev/*.rst"
        "docs/*.md"
        "docs/*.hs"
        "docs/*.svg"
        "docs/API-changelog.txt"
        "docs/streamly-docs.cabal"
        "examples/README.md"
        "src/Streamly/Internal/Data/Stream/Instances.hs"
        "src/Streamly/Internal/Data/Time/Clock/config-clock.h"
        "src/Streamly/Internal/Data/Array/PrimInclude.hs"
        "src/Streamly/Internal/Data/Array/Prim/TypesInclude.hs"
        "src/Streamly/Internal/Data/Array/Prim/MutTypesInclude.hs"
        "src/Streamly/Internal/FileSystem/Event/Darwin.h"
        "src/config.h.in"
        "src/inline.hs"
        "test/README.md"
        "test/Streamly/Test/Common/Array.hs"
        "test/Streamly/Test/Data/*.hs"
        "test/Streamly/Test/Data/Array/Prim.hs"
        "test/Streamly/Test/Data/Array/Prim/Pinned.hs"
        "test/Streamly/Test/Data/Array/Foreign.hs"
        "test/Streamly/Test/Data/Parser/ParserD.hs"
        "test/Streamly/Test/FileSystem/Event.hs"
        "test/Streamly/Test/FileSystem/Handle.hs"
        "test/Streamly/Test/Network/Socket.hs"
        "test/Streamly/Test/Network/Inet/TCP.hs"
        "test/Streamly/Test/Prelude.hs"
        "test/Streamly/Test/Prelude/*.hs"
        "test/Streamly/Test/Unicode/Stream.hs"
        "test/lib/Streamly/Test/Common.hs"
        "test/lib/Streamly/Test/Prelude/Common.hs"
        "test/streamly-tests.cabal"
        "test/version-bounds.hs"
        ];
      extraTmpFiles = [
        "config.log"
        "config.status"
        "autom4te.cache"
        "src/config.h"
        ];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."heaps" or (errorHandler.buildDepError "heaps"))
          (hsPkgs."atomic-primops" or (errorHandler.buildDepError "atomic-primops"))
          (hsPkgs."lockfree-queue" or (errorHandler.buildDepError "lockfree-queue"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."fusion-plugin-types" or (errorHandler.buildDepError "fusion-plugin-types"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))) ++ (pkgs.lib).optionals (flags.inspection) [
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."inspection-testing" or (errorHandler.buildDepError "inspection-testing"))
          ]) ++ (pkgs.lib).optional (flags.dev && flags.inspection) (hsPkgs."inspection-and-dev-flags-cannot-be-used-together" or (errorHandler.buildDepError "inspection-and-dev-flags-cannot-be-used-together"));
        frameworks = (pkgs.lib).optional (system.isOsx) (pkgs."Cocoa" or (errorHandler.sysDepError "Cocoa"));
        buildable = true;
        modules = (([
          "Streamly/Data/Array"
          "Streamly/Data/Prim/Array"
          "Streamly/Data/SmallArray"
          "Streamly/Prelude"
          "Streamly/Data/Unfold"
          "Streamly/Data/Fold"
          "Streamly/Data/Fold/Tee"
          "Streamly/Data/Array/Foreign"
          "Streamly/Unicode/Stream"
          "Streamly/FileSystem/Handle"
          "Streamly/Console/Stdio"
          "Streamly/Network/Socket"
          "Streamly/Network/Inet/TCP"
          "Streamly"
          "Streamly/Data/Unicode/Stream"
          "Streamly/Memory/Array"
          "Streamly/Internal/BaseCompat"
          "Streamly/Internal/Control/Exception"
          "Streamly/Internal/Control/Monad"
          "Streamly/Internal/Control/Concurrent"
          "Streamly/Internal/Data/Cont"
          "Streamly/Internal/Data/Tuple/Strict"
          "Streamly/Internal/Data/Maybe/Strict"
          "Streamly/Internal/Data/Either/Strict"
          "Streamly/Internal/Foreign/Malloc"
          "Streamly/Internal/Data/Atomics"
          "Streamly/Internal/Data/IOFinalizer"
          "Streamly/Internal/Data/Time"
          "Streamly/Internal/Data/Time/TimeSpec"
          "Streamly/Internal/Data/Time/Units"
          "Streamly/Internal/Data/Time/Clock/Type"
          "Streamly/Internal/Data/Time/Clock"
          "Streamly/Internal/Data/SVar"
          "Streamly/Internal/Data/Stream/StreamK/Type"
          "Streamly/Internal/Data/Fold/Type"
          "Streamly/Internal/Data/Stream/StreamD/Step"
          "Streamly/Internal/Data/Stream/StreamD/Type"
          "Streamly/Internal/Data/Stream/StreamDK/Type"
          "Streamly/Internal/Data/Unfold/Type"
          "Streamly/Internal/Data/Producer/Type"
          "Streamly/Internal/Data/Producer"
          "Streamly/Internal/Data/Producer/Source"
          "Streamly/Internal/Data/Sink/Type"
          "Streamly/Internal/Data/Parser/ParserK/Type"
          "Streamly/Internal/Data/Parser/ParserD/Type"
          "Streamly/Internal/Data/Pipe/Type"
          "Streamly/Internal/Data/IORef/Prim"
          "Streamly/Internal/Data/Array/Foreign/Mut/Type"
          "Streamly/Internal/Data/Array/Foreign/Type"
          "Streamly/Internal/Data/Array/Prim/Mut/Type"
          "Streamly/Internal/Data/Array/Prim/Type"
          "Streamly/Internal/Data/Array/Prim/Pinned/Mut/Type"
          "Streamly/Internal/Data/Array/Prim/Pinned/Type"
          "Streamly/Internal/Data/SmallArray/Type"
          "Streamly/Internal/Data/Stream/StreamK"
          "Streamly/Internal/Data/Stream/StreamD/Generate"
          "Streamly/Internal/Data/Stream/StreamD/Eliminate"
          "Streamly/Internal/Data/Stream/StreamD/Nesting"
          "Streamly/Internal/Data/Stream/StreamD/Transform"
          "Streamly/Internal/Data/Stream/StreamD/Exception"
          "Streamly/Internal/Data/Stream/StreamD/Lift"
          "Streamly/Internal/Data/Stream/StreamD"
          "Streamly/Internal/Data/Stream/StreamDK"
          "Streamly/Internal/Data/Stream/Prelude"
          "Streamly/Internal/Data/Parser/ParserD/Tee"
          "Streamly/Internal/Data/Parser/ParserD"
          "Streamly/Internal/Data/Unfold"
          "Streamly/Internal/Data/Fold/Tee"
          "Streamly/Internal/Data/Fold"
          "Streamly/Internal/Data/Sink"
          "Streamly/Internal/Data/Parser"
          "Streamly/Internal/Data/Pipe"
          "Streamly/Internal/Data/Stream/SVar"
          "Streamly/Internal/Data/Stream/Serial"
          "Streamly/Internal/Data/Stream/Async"
          "Streamly/Internal/Data/Stream/Parallel"
          "Streamly/Internal/Data/Stream/Ahead"
          "Streamly/Internal/Data/Stream/Zip"
          "Streamly/Internal/Data/Stream/IsStream/Combinators"
          "Streamly/Internal/Data/Stream/IsStream/Common"
          "Streamly/Internal/Data/Stream/IsStream/Types"
          "Streamly/Internal/Data/Stream/IsStream/Enumeration"
          "Streamly/Internal/Data/Stream/IsStream/Generate"
          "Streamly/Internal/Data/Stream/IsStream/Eliminate"
          "Streamly/Internal/Data/Stream/IsStream/Transform"
          "Streamly/Internal/Data/Stream/IsStream/Expand"
          "Streamly/Internal/Data/Stream/IsStream/Reduce"
          "Streamly/Internal/Data/Stream/IsStream/Exception"
          "Streamly/Internal/Data/Stream/IsStream/Lift"
          "Streamly/Internal/Data/Stream/IsStream/Top"
          "Streamly/Internal/Data/Stream/IsStream"
          "Streamly/Internal/Data/List"
          "Streamly/Internal/Data/Array"
          "Streamly/Internal/Data/Array/Foreign"
          "Streamly/Internal/Data/Array/Prim"
          "Streamly/Internal/Data/Array/Prim/Pinned"
          "Streamly/Internal/Data/SmallArray"
          "Streamly/Internal/Data/Array/Stream/Mut/Foreign"
          "Streamly/Internal/Data/Array/Stream/Foreign"
          "Streamly/Internal/Data/Array/Stream/Fold/Foreign"
          "Streamly/Internal/Ring/Foreign"
          "Streamly/Internal/Unicode/Stream"
          "Streamly/Internal/Unicode/Char"
          "Streamly/Internal/Unicode/Array/Char"
          "Streamly/Internal/Unicode/Array/Prim/Pinned"
          "Streamly/Internal/Data/Binary/Decode"
          "Streamly/Internal/FileSystem/Handle"
          "Streamly/Internal/FileSystem/Dir"
          "Streamly/Internal/FileSystem/File"
          "Streamly/Internal/FileSystem/IOVec"
          "Streamly/Internal/FileSystem/FDIO"
          "Streamly/Internal/FileSystem/FD"
          "Streamly/Internal/Console/Stdio"
          "Streamly/Internal/Network/Socket"
          "Streamly/Internal/Network/Inet/TCP"
          ] ++ (pkgs.lib).optional (system.isWindows) "Streamly/Internal/FileSystem/Event/Windows") ++ (pkgs.lib).optional (system.isOsx) "Streamly/Internal/FileSystem/Event/Darwin") ++ (pkgs.lib).optional (system.isLinux) "Streamly/Internal/FileSystem/Event/Linux";
        cSources = (pkgs.lib).optional (system.isWindows) "src/Streamly/Internal/Data/Time/Clock/Windows.c" ++ (pkgs.lib).optionals (system.isOsx) [
          "src/Streamly/Internal/Data/Time/Clock/Darwin.c"
          "src/Streamly/Internal/FileSystem/Event/Darwin.m"
          ];
        jsSources = [ "jsbits/clock.js" ];
        hsSourceDirs = [ "src" ];
        includeDirs = [
          "src"
          ] ++ (pkgs.lib).optional (system.isOsx) "src/Streamly/Internal";
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././.source-repository-packages/19; }