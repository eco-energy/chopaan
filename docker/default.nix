with import <nixpkgs> {};

let chopaan = import ../default.nix ;
in
  {
  chopaanAppImage = dockerTools.buildImage {
                  name = "chopaan-image";
                  contents = [ chopaan ];

                  config = {
                    Cmd = [ "chopaan-exe" ];
                    ExposedPorts = {
                    "8883/tcp" = {};
                    };
                  };
               };
  }

