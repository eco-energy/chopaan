let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
in
{
  "release" = pkgs.chopaanRelease;
  "pre-commit-hooks" = pre-commit-hooks.run;
  "nixos-module-test" = import ./nix/nixos-module-test.nix {
    inherit sources pkgs;
  }
}
