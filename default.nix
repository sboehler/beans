{ mkDerivation, aeson, base, beam-core, beam-migrate, beam-postgres
, bytestring, cereal, conduit, cryptonite, hpack, microlens-platform
, postgresql-simple, postgresql-simple-migration, resource-pool
, rio, servant-auth, servant-auth-server, servant-conduit, servant-server, stdenv
, time, transformers, wai, warp, pkgs
}:
mkDerivation {
  pname = "waccounts";
  version = "0.1.0.0";
  src = pkgs.nix-gitignore.gitignoreSource [] ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base beam-core beam-migrate beam-postgres bytestring cereal
    conduit cryptonite microlens-platform postgresql-simple
    postgresql-simple-migration resource-pool rio servant-auth
    servant-auth-server servant-conduit servant-server time transformers wai warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base ];
  prePatch = "hpack";
  homepage = "https://github.com/sboehler/web-app#readme";
  license = stdenv.lib.licenses.bsd3;
}
