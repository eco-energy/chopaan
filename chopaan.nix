{ mkDerivation, aeson, amazonka, amazonka-iot, base, brick
, bytestring, Cabal, checkers, concat-classes, concat-examples
, connection, containers, convertible, cursor, data-default-class
, directory, estimator, hashable, hmatrix, hpack, hspec, kalman
, microlens, microlens-th, mtl, net-mqtt, network-uri
, optparse-simple, proto-lens, proto-lens-arbitrary
, proto-lens-protoc, proto-lens-runtime, proto-lens-setup
, QuickCheck, quickcheck-instances, reflection, rio, stdenv, stm
, stm-containers, streamly, text, time, time-lens, tls, ulid
, unordered-containers, vector, vty, x509-store, x509-validation
}:
mkDerivation {
  pname = "chopaan";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal proto-lens-setup ];
  libraryHaskellDepends = [
    aeson amazonka amazonka-iot base brick bytestring checkers
    concat-classes concat-examples connection containers convertible
    cursor data-default-class directory estimator hashable hmatrix
    hspec kalman microlens microlens-th mtl net-mqtt network-uri
    proto-lens proto-lens-arbitrary proto-lens-runtime QuickCheck
    quickcheck-instances reflection rio stm stm-containers streamly
    text time time-lens tls ulid unordered-containers vector vty
    x509-store x509-validation
  ];
  libraryToolDepends = [ hpack proto-lens-protoc ];
  executableHaskellDepends = [
    aeson amazonka amazonka-iot base brick bytestring checkers
    concat-classes concat-examples connection containers convertible
    cursor data-default-class directory estimator hashable hmatrix
    hspec kalman microlens microlens-th mtl net-mqtt network-uri
    optparse-simple proto-lens proto-lens-arbitrary proto-lens-runtime
    QuickCheck quickcheck-instances reflection rio stm stm-containers
    streamly text time time-lens tls ulid unordered-containers vector
    vty x509-store x509-validation
  ];
  executableToolDepends = [ proto-lens-protoc ];
  testHaskellDepends = [
    aeson amazonka amazonka-iot base brick bytestring checkers
    concat-classes concat-examples connection containers convertible
    cursor data-default-class directory estimator hashable hmatrix
    hspec kalman microlens microlens-th mtl net-mqtt network-uri
    proto-lens proto-lens-arbitrary proto-lens-runtime QuickCheck
    quickcheck-instances reflection rio stm stm-containers streamly
    text time time-lens tls ulid unordered-containers vector vty
    x509-store x509-validation
  ];
  testToolDepends = [ proto-lens-protoc ];
  prePatch = "hpack";
  homepage = "https://github.com/faezs/chopaan#readme";
  license = stdenv.lib.licenses.bsd3;
}
