{ mkDerivation, base, bifunctors, bytestring, containers
, exceptions, filepath, groups, hpack, lens, megaparsec, mtl
, optparse-applicative, parser-combinators, prettyprinter
, regex-pcre, stdenv, tasty, tasty-golden, tasty-hunit
, tasty-quickcheck, tasty-smallcheck, text, time
}:
mkDerivation {
  pname = "beans";
  version = "0.0.4.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bifunctors bytestring containers exceptions filepath groups
    lens megaparsec mtl optparse-applicative parser-combinators
    prettyprinter regex-pcre tasty tasty-golden tasty-hunit
    tasty-quickcheck tasty-smallcheck text time
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bifunctors bytestring containers exceptions filepath groups
    lens megaparsec mtl optparse-applicative parser-combinators
    prettyprinter regex-pcre tasty tasty-golden tasty-hunit
    tasty-quickcheck tasty-smallcheck text time
  ];
  testHaskellDepends = [
    base bifunctors bytestring containers exceptions filepath groups
    lens megaparsec mtl optparse-applicative parser-combinators
    prettyprinter regex-pcre tasty tasty-golden tasty-hunit
    tasty-quickcheck tasty-smallcheck text time
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/sboehler/beans#readme";
  description = "A plain text accounting tool";
  license = stdenv.lib.licenses.bsd3;
}
