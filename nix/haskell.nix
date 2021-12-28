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
, compiler ? "ghc8107"
# Enable profiling
, profiling ? config.haskellNix.profiling or false
, cudaSupport ? true
, pkgs
}:

let
  branchmap = {
    "https://github.com/faezs/net-spider.git" = "bidirectional-neighborhood";
    "https://github.com/brendanhay/amazonka.git" = "main";
    "https://github.com/faezs/concat.git" = "graphics-playground";
    };
  cleanGitHaskell = {src, name } :
    let
      clean = haskell-nix.haskellLib.cleanGit { name = "${name}-gitClean"; inherit src; };
    in haskell-nix.cleanSourceHaskell { inherit name; src = clean; };

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.stackProject  {
    src = cleanGitHaskell { name = "chopaan"; src = ../.; };
    compiler-nix-name = compiler;
    #stack-sha256 = "07xcy5j2qir1pnp2g2bznd21z1dfcmv860rzd7nd0iy061iwdi15";
    materialized = ./chopaan.materialized;
    checkMaterialization = false;
    # these extras will provide additional packages
    # ontop of the package set derived from cabal resolution.
    pkg-def-extras = [(hackage: {
      packages = {
        # Win32 = hackage.Win32."2.8.3.0".revisions.default;
        dear-imgui = hackage.dear-imgui."1.3.0".revisions.default;
      };
    })];
    branchMap = branchmap;
    lookupBranch = { location, ... }: (branchmap."${location}" or null);
    modules = [
      {
        compiler.nix-name = compiler;
        #packages.chopaan.configureFlags = [ "--ghc-option=-Werror" ];
        enableLibraryProfiling = profiling;
        doCoverage = false;
      }
      # Add dependencies
      {
        
        packages.chopaan = {
          #package.cleanHpack = true;
          doCheck = false;
          flags.prod = false;
          components.exes.kbtzim.dontStrip = false;
          components.exes.server.dontStrip = false;
          components.library.build-tools = [ buildPackages.z3 ];
          configureFlags = [
            "--extra-lib-dirs=${buildPackages.z3}/lib"
            "--extra-include-dirs=${buildPackages.z3}/include"
            #"--ghc-option=-O1"
          ];
          # components.tests.chopaan-test.build-tools = let
          #   dockerCompat = pkgs.runCommandNoCC "docker-podman-compat" {} ''
          #                  mkdir -p $out/bin
          #                  ln -s ${pkgs.podman}/bin/podman $out/bin/docker
          #                  '';
          # in [
          #   dockerCompat
          #   buildPackages.podman
          #   buildPackages.slirp4netns
          #   buildPackages.runc
          #   buildPackages.conmon
          #   buildPackages.skopeo
          #   buildPackages.fuse-overlayfs
          #   buildPackages.newuidmap
          #   buildPackages.newgidmap
          # ];
          # jormungandr
          # components.tests.chopaan-test.preBuild = let
          #   janusImg = buildPackages.dockerTools.pullImage
          #     { imageName = "janusgraph/janusgraph";
          #       imageDigest = "sha256:a3c3c55922ce882485ac920cf49fff966427846117b41395f6018abcbf8f0839";
          #       sha256 = "1amwhrfjr54vxalx98lb4cvzwgzqslbqfjqpkbbwsr8r2wldgyj1";

          #     };
          # in ''
          # export HOME=`mktemp -d`
          # echo "THIS IS HOME: " $HOME
          # ls -l ${janusImg}
          # cat /proc/self/uid_map
          # cat>/proc/self/uid_map <<EOF
          # 0       1000          1
          # 1     100000      65536
          # EOF
          # ${buildPackages.podman}/bin/podman load < ${janusImg}
          # echo "ima load buzzo!"
          # alias docker=${buildPackages.podman}/bin/podman
          # echo "hey docker! $(docker)"
          # '';
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
        # z3 fixes
        packages.sbv.components.library.libs = pkgs.lib.mkForce
          [ buildPackages.z3 ];

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
        
        .Woah {
         height: auto;
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

        # dont haddock concatisms
        # packages.concat-inline.doHaddock = false;
        # packages.concat-plugin.doHaddock = false;
        # packages.concat-examples.doHaddock = false;
        
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
