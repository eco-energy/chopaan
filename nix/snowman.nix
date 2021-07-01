let
  f =
    build-or-shell:
    { compiler ? "ghc865"
    , withHoogle ? false
    , doHoogle ? false
    , doHaddock ? false
    , enableLibraryProfiling ? false
    , enableExecutableProfiling ? false
    , strictDeps ? false
    , isJS ? false
    , system ? builtins.currentSystem
    , optimize ? true
    , shpadoinkle-path ? null
    }:
    let
      c = "5272327b81ed355bbed5659b8d303cf2979b6953";
      fetchFromGitHub = (import (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/${c}.tar.gz";
      }) { inherit system; }).fetchFromGitHub;
      
      # It's a shpadoinkle day
      shpadoinkle = fetchFromGitHub {
        owner = "Fresheyeball";
        repo = "shpadoinkle";
        rev    = "8ac480f78e0fa8d75d9335dc1e1eed2aa4f9efd4";
        sha256 = "0vvykghlf3d289ni3817zl4h3d49xxwrc6xxcjdygawbsng0v4yf";
      };

      chan = import (shpadoinkle + "/nix/chan.nix");

      # Additional ignore patterns to keep the Nix src clean
      ignorance = [
        "*.md"
        "figlet"
        "*.nix"
        "*.sh"
        "*.yml"
        "result"
      ];


      # Get some utilities
      inherit (import (shpadoinkle + "/nix/util.nix") { inherit compiler isJS pkgs; }) compilerjs gitignore doCannibalize;


      # Build faster by doing less
      chill = p: (pkgs.haskell.lib.overrideCabal p {
        inherit enableLibraryProfiling enableExecutableProfiling;
      }).overrideAttrs (_: {
        inherit doHoogle doHaddock strictDeps;
      });


      # Overlay containing Shpadoinkle packages, and needed alterations for those packages
      # as well as optimizations from Reflex Platform
      shpadoinkle-overlay =
        import (shpadoinkle + "/nix/overlay.nix") { inherit compiler chan isJS enableLibraryProfiling enableExecutableProfiling; };


      concat = fetchFromGitHub {
        owner = "faezs";
        repo = "concat";
        rev  = "58dd9e82914bd1eaa2ff048f5477510892598a00";
        sha256 = "0savkim89mplld2xpfi65j17kdhv5n9dspyggqvvm7lxgivkispq";
      };
      
      fdnSrc = fetchFromGitHub {
        owner = "hamishmack";
        repo = "foundation";
        rev = "421e8056fabf30ef2f5b01bb61c6880d0dfaa1c8";
        sha256 = "0cbsj3dyycykh0lcnsglrzzh898n2iydyw8f2nwyfvfnyx6ac2im";
      };

      streamlySrc = fetchFromGitHub {
        owner = "composewell";
        repo = "streamly";
        rev = "4e48b52e8709390a47e99c107ebed2d1c9420076";
        sha256 = "1824vqnx9ncs8f1vzgl08r5bd274vhabsdkirqqa88fxl0f2jyw5";
      };

      streamly =
        pkgs.haskell.packages.${compilerjs}.callCabal2nix "streamly" streamlySrc {};
      foundation =
        pkgs.haskell.packages.${compilerjs}.callCabal2nix "foundation" (fdnSrc + /foundation) {};


      concatPkg = p:
        pkgs.haskell.packages.${compilerjs}.callCabal2nix ("concat-${p}") (concat + "/${p}") {};

      
      # Haskell specific overlay (for you to extend)
      haskell-overlay = hself: hsuper: {
        "happy" = pkgs.haskell.lib.dontCheck hsuper.happy;
        "digits" = pkgs.haskell.lib.dontCheck hsuper.digits;
        "haxl" = pkgs.haskell.lib.dontCheck hsuper.haxl;
        "concat-inline" = concatPkg "inline";
        "concat-known" = concatPkg "known";
        "concat-satisfy" = concatPkg "satisfy";
        "concat-classes" = concatPkg "classes";
        "concat-plugin" = concatPkg "plugin";
        "concat-examples" = concatPkg "examples";
        "concat-graphics" = concatPkg "graphics";
        "streamly" = streamly;
        "abstract-dequeue" = pkgs.haskell.lib.dontCheck hsuper.abstract-dequeue;
        "lockfree-queue" = pkgs.haskell.lib.dontCheck hsuper.lockfree-queue;
        "http-date" = pkgs.haskell.lib.dontCheck hsuper.http-date;
        "intervals" = pkgs.haskell.lib.dontCheck hsuper.intervals;
        "iproute" = pkgs.haskell.lib.dontCheck hsuper.iproute;
        "bytes" = pkgs.haskell.lib.dontCheck hsuper.bytes;
        "extra" = pkgs.haskell.lib.dontCheck hsuper.extra;
        "ad" = pkgs.haskell.lib.dontCheck hsuper.ad;
        "foundation" = pkgs.haskell.lib.dontCheck foundation;
        "cereal" = pkgs.haskell.lib.dontCheck hsuper.cereal;
        "lens" = pkgs.haskell.lib.dontCheck hsuper.lens;
        "linear" = pkgs.haskell.lib.dontCheck hsuper.linear;
        "compensated" = pkgs.haskell.lib.dontCheck hsuper.compensated;
        "log-domain" = pkgs.haskell.lib.dontCheck hsuper.log-domain;
        "rio" = pkgs.haskell.lib.dontCheck hsuper.rio;
        "exact-pi" = pkgs.haskell.lib.dontCheck hsuper.exact-pi;
        "dimensional" = pkgs.haskell.lib.dontCheck hsuper.dimensional;
        "shelly" = pkgs.haskell.lib.dontCheck hsuper.shelly;
        "fingertree" = pkgs.haskell.lib.dontCheck hsuper.fingertree;
        "diagrams-lib" = pkgs.haskell.lib.dontCheck hsuper.diagrams-lib;
        "generic-deriving" = pkgs.haskell.lib.dontCheck hsuper.generic-deriving;
        "greskell" = pkgs.haskell.lib.dontCheck (hsuper.callHackageDirect {
          pkg = "greskell";
          ver = "1.2.0.0";
          sha256 = "00c62j4bsib7niiix0j9429j4f3yzlrxviz7rb1i46mwnx077b5m";
        } {});
        "network" = pkgs.haskell.lib.dontCheck (hsuper.callHackageDirect {
          pkg = "network";
          ver = "3.1.2.1";
          sha256 = "19wxakzxayq0dmw09w7zh0hbjni24gh7h8ayc9zp4gndypys8zm5";
        } {});
        "greskell-core" = pkgs.haskell.lib.dontCheck (hsuper.callHackageDirect {
          pkg = "greskell-core";
          ver = "0.1.3.5";
          sha256 = "046c25fifs2495hxy5d4j3cpjbj61xzvrwf7xk28j5df09rw4xh2";
        } {});
        "algebraic-graphs" = pkgs.haskell.lib.dontCheck (hsuper.callHackageDirect {
          pkg = "algebraic-graphs";
          ver = "0.5";
          sha256 = "0z8mgzdis72a9zd9x9f185phqr4bx8s06piggis4rlih1rly61nr";
        } {});
        "fusion-plugin-types" = hsuper.callHackageDirect {
          pkg = "fusion-plugin-types";
          ver = "0.1.0";
          sha256 = "17211b80p4zqisghs2j8flm4dj788f9glx2id6nh8f223q4cigc9";
        } {};
        # Entropy has a custom Setup.hs file which needs to be fixed.
        "entropy" = hsuper.callHackageDirect {
          pkg = "entropy";
          ver = "0.4.1.6";
          sha256 = "0iplymcd5p93ibhvg7n4sqwsqzgr2vlcb128r2r12nw75wc2qw52";
        } {};
        "memory" = hsuper.callHackageDirect {
          pkg = "memory";
          ver = "0.14.18";
          sha256 = "17158ibx2q41al8jhx3qybkpb77in7823nwjqig926gfzc4mxaga";
        } {};
        "pcg-random" = pkgs.haskell.lib.dontCheck (hsuper.callHackageDirect {
          pkg = "pcg-random";
          ver = "0.1.3.6";
          sha256 = "120md9vpdbb8x3qidc4m1mn48rlv7zrmw2kpnzyrd19w96s20s26";
        } {});
        "crockford" = pkgs.haskell.lib.dontCheck (hsuper.callHackageDirect {
          pkg = "crockford";
          ver = "0.2";
          sha256 = "0msldq90xdzpfsry968035ii88c0d36cz87myh96if47sz186k5w";
        } {});        
      };


      # Top level overlay (for you to extend)
      chopaan-overlay = self: super: {
        haskell = super.haskell //
          { packages = super.haskell.packages //
            { ${compilerjs} = super.haskell.packages.${compilerjs}.override (old: {
                overrides = super.lib.composeExtensions (old.overrides or (_: _: {})) haskell-overlay;
              });
            };
          };
        };


      # Complete package set with overlays applied
      pkgs = import
        (builtins.fetchTarball {
          url = "https://github.com/NixOS/nixpkgs/archive/${chan}.tar.gz";
        }) {
        inherit system;
        overlays = [
          shpadoinkle-overlay
          chopaan-overlay
        ];
      };


      ghcTools = with pkgs.haskell.packages.${compiler};
        [ cabal-install
          ghcid
        ] ++ (if isJS then [] else [ stylish-haskell ]);


      # We can name him George
      chopaan = pkgs.haskell.packages.${compilerjs}.callCabal2nix "chopaan" (gitignore ignorance ../.) {};


    in with pkgs; with lib;

      { build =
          (if isJS && optimize then doCannibalize else x: x) (chill chopaan);

        shell =
          pkgs.haskell.packages.${compilerjs}.shellFor {
            inherit withHoogle;
            packages    = _: [ chopaan ];
            COMPILER    = compilerjs;
            buildInputs = ghcTools;
            shellHook   = ''
              ${lolcat}/bin/lolcat ${../figlet}
              cat ${../intro}
            '';
          };
      }.${build-or-shell};
in
  { build = f "build";
    shell = f "shell";
  }
