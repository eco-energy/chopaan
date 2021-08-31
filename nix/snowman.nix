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
        rev    = "34cfee8702c8c104a211686e9c6c078315b65c0a";
        sha256 = "14aajxpr14506qj2hxnjy6iz4c2w8ch5dchqhy81a5i1s0nylid6";
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


      chill = p: pkgs.haskell.lib.dontHaddock p;
      # Build faster by doing less
      # chill = p: (pkgs.haskell.lib.overrideCabal p {
      #   inherit enableLibraryProfiling enableExecutableProfiling;
      # }).overrideAttrs (_: {
      #   inherit doHoogle doHaddock strictDeps;
      # });


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
        rev = "4f629b8cb36bd03b480edc08b77c9a0187ce2206";
        sha256 = "1ag6lqr74c1ml0vmai7a1b28dyf149pv3mhpg06kp27sawl71sy2";
      };
      
      streamlyP =
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
        "concat-inline" = pkgs.haskell.lib.dontHaddock (concatPkg "inline");
        "concat-known" = concatPkg "known";
        "concat-satisfy" = concatPkg "satisfy";
        "concat-classes" = concatPkg "classes";
        "concat-plugin" = pkgs.haskell.lib.dontHaddock (concatPkg "plugin");
        "concat-examples" = pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.dontCheck (concatPkg "examples"));
        "concat-graphics" = pkgs.haskell.lib.dontHaddock (pkgs.haskell.lib.dontCheck (concatPkg "graphics"));
        "streamly" = pkgs.haskell.lib.dontCheck streamlyP;
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
        "compensated" = pkgs.haskell.lib.dontCheck hsuper.compensated;
        "log-domain" = pkgs.haskell.lib.dontCheck hsuper.log-domain;
        "rio" = pkgs.haskell.lib.dontCheck hsuper.rio;
        "base64" = pkgs.haskell.lib.dontCheck hsuper.base64;
        "exact-pi" = pkgs.haskell.lib.dontCheck hsuper.exact-pi;
        "dimensional" = pkgs.haskell.lib.dontCheck hsuper.dimensional;
        "shelly" = pkgs.haskell.lib.dontCheck hsuper.shelly;
        "html-parse" = pkgs.haskell.lib.dontCheck hsuper.html-parse;               
        "fingertree" = pkgs.haskell.lib.dontCheck hsuper.fingertree;
        "generic-deriving" = pkgs.haskell.lib.dontCheck hsuper.generic-deriving;
        "streaming-commons" = pkgs.haskell.lib.dontCheck hsuper.streaming-commons;
        "lattices" = pkgs.haskell.lib.dontCheck hsuper.lattices;
        "linear" = pkgs.haskell.lib.dontCheck (hsuper.callHackageDirect {
          pkg = "linear";
          ver = "1.21.6";
          sha256 = "1vyh33k14b6plk5ic2yrkl8z618ivd2gjz6qll2xrsswrkdfwxhr";
        } {});
        "indexed-traversable" = pkgs.haskell.lib.dontCheck (hsuper.callHackageDirect {
          pkg = "indexed-traversable";
          ver = "0.1.1";
          sha256 = "1r5hvz6c90qcjc6r79r1vdv38l898saiv0027xzknlp48hcx8292";
        } {});
        "base-orphans" = pkgs.haskell.lib.dontCheck (hsuper.callHackageDirect {
          pkg = "base-orphans";
          ver = "0.8.3";
          sha256 = "12lgnyg0qd5nvg7cvknv8b2lkm7s0mhbgi60a223nfqwq6684ynl";
        } {});
        "diagrams-core" = pkgs.haskell.lib.dontCheck (hsuper.callHackageDirect {
          pkg = "diagrams-core";
          ver = "1.5.0";
          sha256 = "0aqzb2ka4nqp13mljfxs9g29b06w5f4gax2vmgy00s72d824bnv1";
        } {});
        "diagrams-lib" = pkgs.haskell.lib.dontCheck (hsuper.callHackageDirect {
          pkg = "diagrams-lib";
          ver = "1.4.4";
          sha256 = "13l701nxny5cgqbfqpjpmb2qfxlrifs0ggzxf4baicf9sb85gvh2";
        } {});
        "diagrams-contrib" = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.overrideCabal (hsuper.callHackageDirect {
          pkg = "diagrams-contrib";
          ver = "1.4.4";
          sha256 = "1bnf193lh100fv268mdq2kg51fy9407gd134s4yk6saxlk0bg31g";
        } {}) (_: {
          revision = "2";
          editedCabalFile = "4f767211b35c60a018534c23cee1b00f560df999c111e5a2c1629d6fcac077d6";
        }));
        "diagrams-svg" = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.overrideCabal (hsuper.callHackageDirect {
          pkg = "diagrams-svg";
          ver = "1.4.3";
          sha256 = "0598dfx7c22wnq6qk4cr8fzqj0clwwsvmrmpyxbiyiz149h2digk";
        } {}) (_: {
          revision = "3";
          editedCabalFile = "7b4eefffba2b25267eea25155dded4309a153a965eebf16923d289bb797ac3a6";
        }));
        "force-layout" = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.overrideCabal (hsuper.callHackageDirect {
          pkg = "force-layout";
          ver = "0.4.0.6";
          sha256 = "0km1h9wix5d0zngbdbcdnv9ga8rmmlj5rmr2axdcfay8g01rx6fg";
        } {}) (_: {
          revision = "7";
          editedCabalFile = "4adae4a45a09a9378ea87930f082a968767a2bc95f6b2122e11966c57046a01c";
        }));
        "active" = pkgs.haskell.lib.dontCheck (hsuper.callHackageDirect {
          pkg = "active";
          ver = "0.2.0.15";
          sha256 = "18prfvmn0k6kfn5asqqzinwqfamrxvkfczcymi0gfmbw5h351gdv";
        } {});
        "monoid-extras" = pkgs.haskell.lib.dontCheck (hsuper.callHackageDirect {
          pkg = "monoid-extras";
          ver = "0.6";
          sha256 = "1z9nvhpcfhzahzx3m9x7z0dbnrqb08pqnb1k4h8na3vq0fva8jq7";
        } {});
        "dual-tree" = pkgs.haskell.lib.dontCheck (hsuper.callHackageDirect {
          pkg = "dual-tree";
          ver = "0.2.3.0";
          sha256 = "152ljd2q73fngh5n7z7nz7kqkx507zabqv0nri4yyp5x6l8sqf9x";
        } {});
        "newtype-generics" = pkgs.haskell.lib.dontCheck (hsuper.callHackageDirect {
          pkg = "newtype-generics";
          ver = "0.6";
          sha256 = "1gr9r8xwqxrp2w9qfqqir5y2k166lih5wll59r36qr8apj1w33pz";
        } {});
        "monad-bayes" = pkgs.haskell.lib.dontCheck (hsuper.callHackageDirect {
          pkg = "monad-bayes";
          ver = "0.1.1.0";
          sha256 = "18wxizq5nn61i3b9g8j8g2mb14wmyxwi4w1hdygqc27r55gpbqz2";
        } {});
        "net-spider" = pkgs.haskell.lib.dontCheck (hsuper.callHackageDirect {
          pkg = "net-spider";
          ver = "0.4.3.6";
          sha256 = "1qhm9aw2pqfarc8l28s57vz4rzk2yjaq194iyxhkbj85y8r99xmw";
        } {});
        "greskell" = pkgs.haskell.lib.dontCheck (hsuper.callHackageDirect {
          pkg = "greskell";
          ver = "1.2.0.0";
          sha256 = "00c62j4bsib7niiix0j9429j4f3yzlrxviz7rb1i46mwnx077b5m";
        } {});
        "greskell-websocket" = pkgs.haskell.lib.dontCheck (hsuper.callHackageDirect {
          pkg = "greskell-websocket";
          ver = "0.1.2.5";
          sha256 = "0ycn921xxk1lyrg8hxlaxn2r4m22skpvvbl3v2qkq4ndz1cc2kqv";
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
            # shellHook   = ''
            #   ${lolcat}/bin/lolcat ${../figlet}
            #   cat ${../intro}
            # '';
          };
      }.${build-or-shell};
in
  { build = f "build";
    shell = f "shell";
  }
