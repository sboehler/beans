{ mkDerivation, aeson, base, bifunctors, bytestring, containers
, dhall, directory, exceptions, filepath, groups, hpack
, http-conduit, megaparsec, mtl, optparse-applicative
, parser-combinators, prettyprinter, recursion-schemes, regex-pcre
, scientific, stdenv, tasty, tasty-golden, tasty-hunit
, tasty-quickcheck, tasty-smallcheck, text, time
, unordered-containers
}:
mkDerivation {
  pname = "beans";
  version = "0.0.4.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bifunctors bytestring containers dhall directory
    exceptions filepath groups http-conduit megaparsec mtl
    optparse-applicative parser-combinators prettyprinter
    recursion-schemes regex-pcre scientific tasty tasty-golden
    tasty-hunit tasty-quickcheck tasty-smallcheck text time
    unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bifunctors bytestring containers dhall directory
    exceptions filepath groups http-conduit megaparsec mtl
    optparse-applicative parser-combinators prettyprinter
    recursion-schemes regex-pcre scientific tasty tasty-golden
    tasty-hunit tasty-quickcheck tasty-smallcheck text time
    unordered-containers
  ];
  testHaskellDepends = [
    aeson base bifunctors bytestring containers dhall directory
    exceptions filepath groups http-conduit megaparsec mtl
    optparse-applicative parser-combinators prettyprinter
    recursion-schemes regex-pcre scientific tasty tasty-golden
    tasty-hunit tasty-quickcheck tasty-smallcheck text time
    unordered-containers
  ];
  prePatch = "hpack";
  homepage = "https://github.com/sboehler/beans#readme";
  description = "A plain text accounting tool";
  license = stdenv.lib.licenses.bsd3;
}
