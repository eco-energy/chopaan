{
  extras = hackage:
    {
      packages = {
        "net-mqtt" = (((hackage.net-mqtt)."0.7.0.1").revisions).default;
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
        "streamly-bytestring" = (((hackage.streamly-bytestring)."0.1.0.1").revisions)."aa522c22f992e70d005acbd7d4dba555b6a8b8f43f29cddba16fbeeb39f3e992";
        "language-glsl" = (((hackage.language-glsl)."0.3.0").revisions)."85c1e7bf2cf5d6e604b7a2899c27e2935033425944db200798e57849e64d4c81";
        "numeric-limits" = (((hackage.numeric-limits)."0.1.0.0").revisions)."214bb53112bff315e42c0e9efa4a89e2cc2e9914e6232d923b596f29f9b1afe6";
        "total-map" = (((hackage.total-map)."0.1.3").revisions)."081b917276323564e6d6ebaaf20c14d8ffc0a260cf3f29113c2577f774a272f1";
        "semiring-num" = (((hackage.semiring-num)."1.6.0.4").revisions)."ea73b7ec4980add625dedfe159dbeea9a37aa540a2a9df0f36263455b1654cc1";
        "key" = (((hackage.key)."0.1.2.0").revisions)."3bdfda94f99b8f2e01498ddf5f704cd84ce93315b532df50420ded94cbf5ba2e";
        "streamly" = (((hackage.streamly)."0.7.2").revisions)."173a415316e230e2117365dcd0432f1a7992d42c89b79017df57c9f6581205e3";
        "fusion-plugin-types" = (((hackage.fusion-plugin-types)."0.1.0").revisions)."0f11bbc445ab8ae3dbbb3d5d2ea198bdb1ac020518b7f4f7579035dc89182438";
        "reflex-vty" = (((hackage.reflex-vty)."0.1.4.0").revisions)."46dcb043c39532e85c08e7098ac2e1237d5f7d018183b3f758f5ef87e06f51d4";
        "bimap" = (((hackage.bimap)."0.3.3").revisions)."232518c0410990665b9c8677eb9318ee355c001d58945ddcbedec3baa30b4160";
        "ref-tf" = (((hackage.ref-tf)."0.4.0.2").revisions)."69de3550250e0cd69f45d080359cb314a9487c915024349c75b78732bbee9332";
        "reflex" = (((hackage.reflex)."0.7.1.0").revisions)."6d224466f0daabd44fef0fa559a09151126452bf857aeaa37692bccb259d5c5d";
        "constraints-extras" = (((hackage.constraints-extras)."0.3.0.2").revisions)."013b8d0392582c6ca068e226718a4fe8be8e22321cc0634f6115505bf377ad26";
        "monoidal-containers" = (((hackage.monoidal-containers)."0.6.0.1").revisions)."7d776942659eb4d70d8b8da5d734396374a6eda8b4622df9e61e26b24e9c8e40";
        "patch" = (((hackage.patch)."0.0.3.1").revisions)."f14acf2eea8c83be57398106cec549c476577a947a7f856e6aa71dc561d58ca9";
        "witherable" = (((hackage.witherable)."0.3.1").revisions)."ed3d5bc9eb1c08fa9704d9e143cdf622e1bd80b847cd5ede8c07da7bc7981ab9";
        "dependent-map" = (((hackage.dependent-map)."0.3.1.0").revisions)."f33391e51264aab38b11d581bb8d2f7c6fc9fcf012bdbb6122708c23b3360b2a";
        "dependent-sum" = (((hackage.dependent-sum)."0.6.2.0").revisions)."bff37c85b38e768b942f9d81c2465b63a96076f1ba006e35612aa357770807b6";
        "monad-bayes" = (((hackage.monad-bayes)."0.1.1.0").revisions)."57d96f530a37a1c8efed0830c7f2bcf86da05d0d53324aebdd5b5aa165d72829";
        "geodetics" = (((hackage.geodetics)."0.1.0").revisions)."699c10cd8d69222b125d054cdc8e2bcee7cb0558c78fa1b4fa06b2d3ecf29f5c";
        "hgeometry" = (((hackage.hgeometry)."0.9.0.0").revisions)."43043574d2ce39c543dfadb629ab3a41fe65d7bf68a1426d8d01312818bc6e4e";
        "hgeometry-combinatorial" = (((hackage.hgeometry-combinatorial)."0.9.0.0").revisions)."a03b48302d8c542ff2ff90ba5d45f2742cc21ed1a676c8fa4870cd461a273354";
        "fusion-plugin" = (((hackage.fusion-plugin)."0.2.1").revisions)."f72d7393d2c39909050cae46173e0218bf533a899be05b448ae55f9d4a81be9c";
        "perfect-vector-shuffle" = (((hackage.perfect-vector-shuffle)."0.1.1.1").revisions)."8656a3f1f491d1d79aba9e0a5dbb08589613202b08c7a9b92570d6bec7390093";
        "servant-websockets" = (((hackage.servant-websockets)."2.0.0").revisions)."6e9e3600bced90fd52ed3d1bf632205cb21479075b20d6637153cc4567000234";
        chopaan = ./chopaan.nix;
        mgenv = ./mgenv.nix;
        concat-inline = ./concat-inline.nix;
        concat-known = ./concat-known.nix;
        concat-satisfy = ./concat-satisfy.nix;
        concat-classes = ./concat-classes.nix;
        concat-plugin = ./concat-plugin.nix;
        concat-examples = ./concat-examples.nix;
        concat-graphics = ./concat-graphics.nix;
        concat-hardware = ./concat-hardware.nix;
        opt-expect = ./opt-expect.nix;
        lCirc = ./lCirc.nix;
        propagators = ./propagators.nix;
        };
      };
  resolver = "lts-14.17";
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "concat-examples" = {
            flags = { "smt" = lib.mkOverride 900 false; };
            };
          };
        })
    { packages = {}; }
    ];
  }