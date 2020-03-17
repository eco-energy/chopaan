{
  extras = hackage:
    {
      packages = {
        "amazonka" = (((hackage.amazonka)."1.6.1").revisions)."d863557379350ed6bbb91187abeb8c349a654f60b140d138710486f58ae7c476";
        "amazonka-iot" = (((hackage.amazonka-iot)."1.6.1").revisions)."b15ae9efd7b35a8817a4578a68baa22202b42946381862053f68e1417a189ae4";
        "amazonka-core" = (((hackage.amazonka-core)."1.6.1").revisions)."9bc59ce403c6eeba3b3eaf3f10e5f0b6a33b6edbbf8f6de0dd6f4c67b86fa698";
        "ulid" = (((hackage.ulid)."0.2.0.0").revisions)."ac415298b0272479909ca576f4444862ae942ed85c7849fa0675e3bf496be6f3";
        "crockford" = (((hackage.crockford)."0.2").revisions)."6ec593711a20bd4b81a0fc13f3d850d2d890690941d777230fbf980c483e434a";
        "cursor" = (((hackage.cursor)."0.2.0.0").revisions)."9071e1029efc634bab63c8b3fd970bbd133475749ec918138a9e7304b0a7e696";
        "stm-containers" = (((hackage.stm-containers)."1.1.0.4").revisions)."f83a683357b6e3b1dda3e70d2077a37224ed534df1f74c4e11f3f6daa7945c5b";
        "stm-hamt" = (((hackage.stm-hamt)."1.2.0.4").revisions)."7957497c022554b7599e790696d1a3e56359ad99e5da36a251894c626ca1f60a";
        "primitive" = (((hackage.primitive)."0.7.0.0").revisions)."c45abc68bec080e3f1ab347dd331617d43fded94a473086bf21aeda69a6e20bc";
        "primitive-extras" = (((hackage.primitive-extras)."0.8").revisions)."fca0310150496867f5b9421fe1541ecda87fae17eae44885a29f9c52dd00c8ff";
        "primitive-unlifted" = (((hackage.primitive-unlifted)."0.1.3.0").revisions)."a98f827740f5dcf097d885b3a47c32f4462204449620abc9d51b8c4f8619f9e6";
        "random-fu-multivariate" = (((hackage.random-fu-multivariate)."0.1.2.1").revisions)."b8b3adcdb01269480b3d28dae1e9c9a040834b9974c434d6996d3e1545c4d4f7";
        "estimator" = (((hackage.estimator)."1.2.0.0").revisions)."44c85267299ea1e3eeb3e4eb9f317e4972563c84ba8220924c834a99b9048960";
        "streamly" = (((hackage.streamly)."0.7.0").revisions)."2a510275b3a41f1e50d8f45f8dac879933139652509dd6cd98b69a5d24e9c55d";
        chopaan = ./chopaan.nix;
        concat-inline = ./concat-inline.nix;
        concat-known = ./concat-known.nix;
        concat-satisfy = ./concat-satisfy.nix;
        concat-classes = ./concat-classes.nix;
        concat-plugin = ./concat-plugin.nix;
        concat-examples = ./concat-examples.nix;
        concat-graphics = ./concat-graphics.nix;
        concat-hardware = ./concat-hardware.nix;
        kalman = ./kalman.nix;
        net-mqtt = ./net-mqtt.nix;
        };
      };
  resolver = "lts-14.17";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }