{ system ? builtins.currentSystem
, optimize ? true
, tinkerHost ? "localhost"
, tinkerPort ? "8182"
}:

let
  artifact = (import ../default.nix { inherit system; }).chopaan.dashgen;
  pkgs = import ./default.nix { inherit system; };
  dash = ../dash;
  #jslib = ../js;
  #asset = ../lab_dash.json;
in
pkgs.runCommand "dashboards" {
  LANG = "C.UTF-8";
} ''
  mkdir $out
  cp -r ${dash} $out
  echo ${pkgs.lib.commitIdFromGitRepo ../.git} > $out/version
  ''


  # ${artifact}/bin/dashgen --tinkerHost ${tinkerHost} --tinkerPort ${tinkerPort} --outpath $out
