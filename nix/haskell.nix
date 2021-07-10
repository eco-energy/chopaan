############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################

{ lib
, stdenv
, haskell-nix
, buildPackages
# Pass in any extra programs necessary for the build as function arguments.
# TODO: Declare packages required by the build.
# jormungandr and cowsay are just examples and should be removed for your
# project, unless needed.
#, makeWrapper
#, jormungandr
#, cowsay

, config ? {}
# GHC attribute name
, compiler ? "ghc865"
# Enable profiling
, profiling ? config.haskellNix.profiling or false
, cudaSupport ? true
, pkgs
}:

let
  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject  {
    src = haskell-nix.haskellLib.cleanGit { name = "chopaan"; src = ../.; };
    compiler-nix-name = compiler;
    index-state = "2021-02-01T00:00:00Z";
    #plan-sha256 = "09yapalp7q5l6fmjsrywaqmxm2frx1y652ccsgjg1nk62d90wbri";
    # these extras will provide additional packages
    # ontop of the package set derived from cabal resolution.
    pkg-def-extras = [(hackage: {
      packages = {
          # Win32 = hackage.Win32."2.8.3.0".revisions.default;
      };
    })];

    modules = [
      {
        compiler.nix-name = compiler;
        #packages.chopaan.configureFlags = [ "--ghc-option=-Werror" ];
        enableLibraryProfiling = profiling;
        doCoverage = false;
         # Fixes for libtorch-ffi
        packages.libtorch-ffi = {
          configureFlags = [
            "--extra-lib-dirs=${buildPackages.torch_cuda}/lib"
            "--extra-include-dirs=${buildPackages.torch_cuda}/include"
            "--extra-include-dirs=${buildPackages.torch_cuda}/include/torch/csrc/api/include"
          ];
          flags = {
            cuda = cudaSupport;
            gcc = !cudaSupport && pkgs.stdenv.hostPlatform.isDarwin;
          };
        };
      }

      # Add dependencies
      {
        
        packages.chopaan = {
          #components.tests.chopaan-tests.build-tools = [ ]; # jormungandr
          doCoverage = false;
          # How to set environment variables for builds
          #preBuild = "export NETWORK=testnet";

          # How to add program depdendencies for benchmarks
          # TODO: remove if not applicable
          #components.benchmarks.chopaan-bench = {
            #build-tools = [ makeWrapper ];
            #postInstall = ''
            #  makeWrapper \
            #    $out/bin/chopaan-bench \
            #    $out/bin/chopaan-bench-wrapped \
            #    --prefix PATH : ${cowsay}/bin
            #'';
          #};

          # fixme: Workaround for https://github.com/input-output-hk/haskell.nix/issues/207
          # components.all.postInstall = lib.mkForce "";
        };
      }

      # Misc. build fixes for dependencies
      {

        # Disable shpadoinkle tests
        packages.Shpadoinkle-html.components.tests.doCheck = false;
        # create fake sample.css file
        packages.Shpadoinkle-html.components.library.preBuild = ''
        cat << EOF > sample.css
        .txt-rt, span.foo{
          text-align:right;
        }
        
        .pos-relative{
        position:relative;
        }

        #foo[type="bar"]{
        background: #123233;
        }

        @media print (min-width:200px){
          .bar{
             width: #EFEFEF;
             content: '.qux and #stuff';
          }
        }
        EOF
        '';

        # Katip has Win32 (>=2.3 && <2.6) constraint
        packages.katip.doExactConfig = true;

        # split data output for ekg to reduce closure size
        packages.ekg.components.library.enableSeparateDataOutput = true;
        
        # some packages are missing identifier.name:
        packages.cryptonite-openssl.package.identifier.name = "cryptonite-openssl";
        packages.file-embed-lzma.package.identifier.name = "file-embed-lzma";
        packages.singletons.package.identifier.name = "singletons";
        packages.terminfo.package.identifier.name = "terminfo";
        packages.conduit.package.identifier.name = "conduit";
        packages.ekg.package.identifier.name = "ekg";
        #packages.streamly.package.identifier.name = "streamly";
      }

      (lib.optionalAttrs stdenv.hostPlatform.isGhcjs {
        # Disable cabal-doctest tests by turning off custom setups
        packages.comonad.package.buildType = lib.mkForce "Simple";
        #packages.compensated.package.buildType = lib.mkForce "Simple";
        packages.distributive.package.buildType = lib.mkForce "Simple";
        packages.lens.package.buildType = lib.mkForce "Simple";
        packages.nonempty-vector.package.buildType = lib.mkForce "Simple";
        packages.semigroupoids.package.buildType = lib.mkForce "Simple";
        packages.free.package.buildType = lib.mkForce "Simple";
        packages.network.package.buildType = lib.mkForce "Simple";
        #packages.streamly.package.buildType = lib.mkForce "Simple";
        packages.streamly.package.doCheck = false;
        packages.streamly.package.doHaddock = false;
        #packages.streamly.flags = {
        #    fusion-plugin = false;
        #    inspection = false;
        #    debug = false;
        #    dev = false;
        #    has-llvm = false;
        #    streamk = false;
        #    examples = false;
        #    examples-sdl = false;
        #  };
        packages.newtype-generics.package.doHaddock = false;
        packages.numtype-dk.package.doHaddock = false;
        packages.MemoTrie.package.doHaddock = false;
        packages.exact-pi.package.doHaddock = false;
        #packages.old-locale.package.doHaddock = false;
        packages.parallel.package.doHaddock = false;
        packages.data-default.package.doHaddock = false;
        #packages.data-default-old-locale.package.doHaddock = false;
        packages.integration.package.doHaddock = false;
        # Make sure we use a buildPackages version of happy
        packages.pretty-show.components.library.build-tools = [ buildPackages.haskell-nix.haskellPackages.happy ];


        # Remove hsc2hs build-tool dependencies (suitable version will be available as part of the ghc derivation)
        #packages.network.package.doCheck = false;
        packages.network.components.library.build-tools = lib.mkForce [];
        packages.streamly.components.library.build-tools = lib.mkForce [];
      })
    ];
  };

in
  pkgSet
