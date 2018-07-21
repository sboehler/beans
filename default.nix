{ mkDerivation, base, bifunctors, bytestring, containers
, exceptions, filepath, groups, megaparsec, mtl
, optparse-applicative, prettyprinter, regex-pcre, scientific
, stdenv, tasty, tasty-hunit, tasty-quickcheck, tasty-smallcheck
, text, time
}:
mkDerivation {
  pname = "beans";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bifunctors bytestring containers exceptions filepath groups
    megaparsec mtl optparse-applicative prettyprinter regex-pcre
    scientific text time
  ];
  testHaskellDepends = [
    base bifunctors bytestring containers exceptions filepath groups
    megaparsec mtl optparse-applicative prettyprinter regex-pcre
    scientific tasty tasty-hunit tasty-quickcheck tasty-smallcheck text
    time
  ];
  homepage = "https://github.com/sboehler/beans#readme";
  description = "A plain text accounting tool";
  license = stdenv.lib.licenses.bsd3;
}
