# chopaan Nix build
#
# fixme: document top-level attributes and how to build them
#
############################################################################

{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize ghc and profiling (see ./nix/haskell.nix):
, config ? {}
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of nixpkgs and iohk-nix:
# nix build -f default.nix chopaan --arg sourcesOverride '{
#   iohk-nix = ../iohk-nix;
#   nixpkgs  = ../nixpkgs;
# }'
, sourcesOverride ? {}
# pinned version of nixpkgs augmented with iohk overlays.
, pkgs ? import ./nix {
    inherit system crossSystem config sourcesOverride;
  }
}:
# commonLib include iohk-nix utilities, our util.nix and nixpkgs lib.
with pkgs; with commonLib;
let
  cacheCreds = splitString ":" (builtins.readFile ./credentials/chopaan-build);
  add-build-cache = 
      import 
        ./nix/nix-build-cache.nix 
        {  
          pkgs = pkgs;
          aws-key = elemAt cacheCreds 0;
          aws-secret = elemAt cacheCreds 1;
          aws-region = "ap-southeast-1";
          cache-name = "chopaan";
          master-cache = "chopaan";
          cache-dirs = [ "dist" ];
          s3-bucket = "chopaan-build";
        };
  recRecurseIntoAttrsAndCache = x:
        if (isAttrs x && !isDerivation x && x.recurseForDerivations or true)
        then recurseIntoAttrs (mapAttrs (n: v: if n == "buildPackages" then (withCache v) else recRecurseIntoAttrsAndCache v) x)
        else (if isDerivation x then add-build-cache x else x);
  withCache = attrs: lib.mapDerivationAttrset add-build-cache attrs;
  haskellPackages = (recRecurseIntoAttrs #AndCache
    # the Haskell.nix package set, reduced to local packages.
    (selectProjectPackages chopaanHaskellPackages));
  
  self = {
    inherit haskellPackages;

    version = (haskellPackages.chopaan.identifier);
    # Grab the executable component of our package.
    #  
    chopaan = (haskellPackages.chopaan.components.exes);

    passthru = (chopaanHaskellPackages.plan-nix.passthru);
    gcroot = pkgs.haskell-nix.roots "ghc865";
    projectCross = (chopaanHaskellPackages.projectCross);
    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    #benchmarks = collectComponents' "benchmarks" haskellPackages;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks haskellPackages;
    };

    shell = import ./shell.nix {
      inherit pkgs;
      withHoogle = true;
    };
    lib = commonLib;
    haskell-nix = haskell-nix;
    #website = import ./nix/website.nix {};
    # Attrset of PDF builds of LaTeX documentation.
    #docs = pkgs.callPackage ./docs/default.nix {};
  };
in
  self




/*
{
  # Fetch the latest haskell.nix and import its default.nix
  sources ? import ./nix/sources.nix
, haskellNix ? import sources."haskell.nix" {}
# haskell.nix provides access to the nixpkgs pins which are used by our CI,
# hence you will be more likely to get cache hits when using these.
# But you can also just use your own, e.g. '<nixpkgs>'.
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2003

# haskell.nix provides some arguments to be passed to nixpkgs, including some
# patches and also the haskell.nix functionality itself as an overlay.
, nixpkgsArgs ? haskellNix.nixpkgsArgs

# import nixpkgs with overlays
, pkgs ? import nixpkgsSrc nixpkgsArgs
, isJS ? false
}:

pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "chopaan";
    src = ./.;
  };
}
*/
