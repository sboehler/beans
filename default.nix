{ mkDerivation, aeson, base, beam-core, beam-migrate, beam-sqlite
, bytestring, cereal, conduit, cryptonite, dhall, hpack, jose
, microlens-platform, optparse-applicative, resource-pool, rio
, rio-orphans, servant, servant-auth, servant-auth-server
, servant-conduit, servant-server, sqlite-simple, stdenv, text
, time, transformers, wai, warp
}:
mkDerivation {
  pname = "waccounts";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base beam-core beam-migrate beam-sqlite bytestring cereal
    conduit cryptonite dhall jose microlens-platform
    optparse-applicative resource-pool rio rio-orphans servant
    servant-auth servant-auth-server servant-conduit servant-server
    sqlite-simple text time transformers wai warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base ];
  prePatch = "hpack";
  homepage = "https://github.com/sboehler/web-app#readme";
  license = stdenv.lib.licenses.bsd3;
}
