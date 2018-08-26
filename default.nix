{ mkDerivation, base, bifunctors, bytestring, containers
, exceptions, filepath, groups, megaparsec, mtl
, optparse-applicative, prettyprinter, regex-pcre, scientific
, stdenv, tasty, tasty-golden, tasty-hunit, tasty-quickcheck
, tasty-smallcheck, text, time
}:
mkDerivation {
  pname = "beans";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bifunctors bytestring containers exceptions filepath groups
    megaparsec mtl optparse-applicative prettyprinter regex-pcre
    scientific text time
  ];
  executableHaskellDepends = [ base optparse-applicative ];
  testHaskellDepends = [
    base bytestring megaparsec tasty tasty-golden tasty-hunit
    tasty-quickcheck tasty-smallcheck
  ];
  homepage = "https://github.com/sboehler/beans#readme";
  description = "A plain text accounting tool";
  license = stdenv.lib.licenses.bsd3;
}
