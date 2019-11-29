{ mkDerivation, aeson, base, beam-core, beam-sqlite, bytestring
, cereal, containers, hpack, microlens-platform
, optparse-applicative, resource-pool, rio, rio-orphans, servant
, servant-server, sqlite-simple, stdenv, text, time, transformers
, wai, warp
}:
mkDerivation {
  pname = "beans-web";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base beam-core beam-sqlite bytestring cereal containers
    microlens-platform resource-pool rio rio-orphans servant
    servant-server sqlite-simple text time transformers wai warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base optparse-applicative ];
  prePatch = "hpack";
  homepage = "https://github.com/sboehler/web-app#readme";
  license = stdenv.lib.licenses.bsd3;
}
