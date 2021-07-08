{ chan ? "5272327b81ed355bbed5659b8d303cf2979b6953"
, system ? builtins.currentSystem
, optimize ? true
}:

let
  artifact = (import ./snowman.nix).build { isJS = true; };
  pkgs = (import (builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/${chan}.tar.gz";
  }) { inherit system; });
  file = if optimize then "all.min.js" else "all.js";
  jslib = ../js;
  assets = ../assets;
in
pkgs.runCommand "website" {
  LANG = "C.UTF-8";
} ''
  mkdir $out
  mkdir $out/assets
  mkdir $out/js
  for src in ${jslib}/*
  do
    cp $src $out/js/$(basename $src)
  done
  for asset in ${assets}/*
  do
    cp $asset $out/assets/$(basename $asset)
  done
  cp ${artifact}/bin/ui.jsexe/${file} $out/all.min.js
  cp ${artifact}/bin/ui.jsexe/index.html $out/index.html
  echo ${pkgs.lib.commitIdFromGitRepo ../.git} > $out/version
  ''
