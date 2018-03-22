{ mkDerivation, base, containers, Decimal, exceptions, filepath
, free, lens, mtl, parsec, parsec3-numbers, prettyprinter, stdenv
, text, time, apply-refact, hlint, hasktags, hoogle, stylish-haskell, hindent
}:
mkDerivation {
  pname = "haricot";
  version = "0.1.0.0";
  src = ./.;
  isExecutable = true;
  buildDepends = [apply-refact hlint hasktags hoogle stylish-haskell hindent];
  executableHaskellDepends = [
    base containers Decimal exceptions filepath free lens mtl parsec
    parsec3-numbers prettyprinter text time
  ];
  homepage = "https://github.com/sboehler/haricot#readme";
  description = "A Haskell parser for beancount";
  license = stdenv.lib.licenses.bsd3;
}
