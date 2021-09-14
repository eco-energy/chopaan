{ system ? builtins.currentSystem
, optimize ? true
}:

let
  #artifact = import ../default.nix { inherit system; }.chopaan.dashboards;
  pkgs = import ./default.nix { inherit system; };
  file = "chopaan-dash.json";
  #jslib = ../js;
  asset = ../lab_dash.json;
in
pkgs.runCommand "dashboards" {
  LANG = "C.UTF-8";
} ''
  mkdir $out
  cp ${asset} $out/$(basename ${asset})
  echo ${pkgs.lib.commitIdFromGitRepo ../.git} > $out/version
  ''
