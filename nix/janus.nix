{
  dockerTools ? (import ./default.nix {}).buildPackages.dockerTools 
}:

let
  janusImg = dockerTools.pullImage
    { imageName = "janusgraph/janusgraph";
      imageDigest = "sha256:a3c3c55922ce882485ac920cf49fff966427846117b41395f6018abcbf8f0839";
      sha256 = "1amwhrfjr54vxalx98lb4cvzwgzqslbqfjqpkbbwsr8r2wldgyj1";
    };
in janusImg
