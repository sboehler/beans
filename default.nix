{ mkDerivation, base, containers, exceptions, filepath
, free, lens, megaparsec, mtl, parsec, parsec3-numbers
, prettyprinter, scientific, stdenv, text, time
}:
mkDerivation {
  pname = "haricot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers exceptions filepath free lens megaparsec
    mtl prettyprinter scientific text time
  ];
  homepage = "https://github.com/sboehler/haricot#readme";
  description = "A Haskell parser for beancount";
  license = stdenv.lib.licenses.bsd3;
}
