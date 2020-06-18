{ nixpkgs ? import (builtins.fetchGit {
  # Descriptive name to make the store path easier to identify
  name = "nixos-19.09";
  url = "https://github.com/nixos/nixpkgs-channels/";
  # Commit hash for nixos-unstable as of 2018-09-12
  # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`
  ref = "refs/heads/nixos-19.09";
  rev = "8260cd5bc65fad88ad8881b32c990e5659cc417a";
}) {} }:

with nixpkgs;

haskell.lib.buildStackProject {
  name = "chopaan";
  src = ./.;
  buildInputs = [ protobuf zlib ];
}
