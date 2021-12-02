{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, sourcesOverride ? {}
, cudaSupport ? true
, cudaMajorVersion ? "11"
}:
let
  sources = import ./sources.nix { inherit pkgs; }
    // sourcesOverride;
  iohKNix = import sources.iohk-nix {};
  haskellNix = import sources."haskell.nix" {
    inherit system;
    sourcesOverride = {
      hackage = sources.hackage-nix;
      stackage = sources.stackage-nix;
    };
  };
  # use our own nixpkgs if it exist in our sources,
  # otherwise use iohkNix default nixpkgs.
  nixpkgs = haskellNix.sources.nixpkgs-unstable or
    (builtins.trace "Using IOHK default nixpkgs" iohKNix.nixpkgs);

  nixUnstable = import (haskellNix.sources.nixpkgs-unstable) {};
  newPodman = nixUnstable.podman;
  newPodmanUnwrapped = nixUnstable.podman-unwrapped;

  # Embed a default signing policy to work around https://github.com/containers/libpod/issues/6053
  overridePodman = drv: drv.overrideAttrs(old: let
    defaultPolicyFile = pkgs.runCommand "skopeo-default-policy.json" {} "cp ${pkgs.skopeo.src}/default-policy.json $out";    vendorPath = "${old.goPackagePath}/vendor/github.com/containers/image/v5";
  in rec {
    postPatch = ''
      for f in $(grep -lri /etc/containers/policy.json); do
         sed -i -e "s#/etc/containers/policy.json#${defaultPolicyFile}#g" "$f"
       done
    '';
  });
  

  podmanOverlay = [
    (pkgs: _: with pkgs; {
      podman = (newPodman.override { podman-unwrapped = (overridePodman newPodmanUnwrapped); });
    })
  ];
  hasktorchOverlays = [
      (pkgs: _: with pkgs;
        let libtorchSrc = callPackage "${sources.pytorch-world}/libtorch/release.nix" { }; in
        if cudaSupport && cudaMajorVersion == "9" then
          let libtorch = libtorchSrc.libtorch_cudatoolkit_9_2; in
          {
            c10 = libtorch;
            torch = libtorch;
            torch_cpu = libtorch;
            torch_cuda = libtorch;
          }
        else if cudaSupport && cudaMajorVersion == "10" then
          let libtorch = libtorchSrc.libtorch_cudatoolkit_10_2; in
          {
            c10 = libtorch;
            torch = libtorch;
            torch_cpu = libtorch;
            torch_cuda = libtorch;
          }
        else if cudaSupport && cudaMajorVersion == "11" then
          let libtorch = libtorchSrc.libtorch_cudatoolkit_11_0; in
          {
            c10 = libtorch;
            torch = libtorch;
            torch_cpu = libtorch;
            torch_cuda = libtorch;
          }
        else
          let libtorch = libtorchSrc.libtorch_cpu; in
          {
            c10 = libtorch;
            torch = libtorch;
            torch_cpu = libtorch;
          }
      )
  ];

  
  # stackhack = [
  #     (pkgsNew: pkgsOld: let inherit (pkgsNew) lib; in {
  #       haskell-nix = pkgsOld.haskell-nix // {
  #         hackageSrc = sources.hackage-nix;
  #         stackageSrc = sources.stackage-nix;
  #       };
  #     })   
  # ];
  
  # for inclusion in pkgs:
  overlays =
    # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
    haskellNix.overlays
    # haskell-nix.haskellLib.extra: some useful extra utility functions for haskell.nix
    ++ iohKNix.overlays.haskell-nix-extra
    # iohkNix: nix utilities and niv:
    ++ iohKNix.overlays.iohkNix
    # hasktorch
    ++ hasktorchOverlays
    ++ podmanOverlay
    # our own overlays:
    ++ [
      (pkgs: _: with pkgs; {

        # commonLib: mix pkgs.lib with iohk-nix utils and our own:
        commonLib = lib // iohkNix
          // import ./util.nix { inherit haskell-nix; }
          # also expose our sources and overlays
          // { inherit overlays sources; };

        # Example of using a package from iohk-nix
        # TODO: Declare packages required by the build.
        # inherit (iohkNix.jormungandrLib.packages.release) jormungandr;
      })
      # Our haskell-nix-ified cabal project:
      (import ./pkgs.nix)
    ];

  pkgs = import nixpkgs (haskellNix.nixpkgsArgs // {
    inherit system crossSystem overlays;
  });

in pkgs
