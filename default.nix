{ mkDerivation, aeson, base, bytestring, cereal, cryptonite
, exceptions, freer-simple, hpack, lens, mtl
, natural-transformation, postgresql-simple
, postgresql-simple-migration, resource-pool, resourcet
, servant-auth, servant-auth-server, servant-server, stdenv, text
, transformers, wai, warp
}:
mkDerivation {
  pname = "servant-starter-app";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring cereal cryptonite exceptions freer-simple
    lens mtl natural-transformation postgresql-simple
    postgresql-simple-migration resource-pool resourcet servant-auth
    servant-auth-server servant-server text transformers wai warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base ];
  preConfigure = "hpack";
  homepage = "https://github.com/sboehler/servant-starter-app#readme";
  license = stdenv.lib.licenses.bsd3;
}
