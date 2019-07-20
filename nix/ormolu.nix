{ mkDerivation, base, containers, dlist, exceptions, fetchgit
, filepath, ghc, ghc-boot-th, ghc-paths, gitrev, hspec
, hspec-discover, mtl, optparse-applicative, path, path-io, stdenv
, syb, text
}:
mkDerivation {
  pname = "ormolu";
  version = "0.0.1.0";
  src = fetchgit {
    url = "https://github.com/tweag/ormolu";
    sha256 = "00hm68n28lwqivp6nz5kc8hx1f18kv6z4x9cfj5skca884i8af2r";
    rev = "63478a0ba40243cf0149c247340798d67476acad";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base containers dlist exceptions ghc ghc-boot-th ghc-paths mtl syb
    text
  ];
  executableHaskellDepends = [
    base ghc gitrev optparse-applicative text
  ];
  testHaskellDepends = [
    base containers filepath hspec path path-io text
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/tweag/ormolu";
  description = "A formatter for Haskell source code";
  license = stdenv.lib.licenses.bsd3;
}
