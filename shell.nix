{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bifunctors, containers, exceptions
      , filepath, megaparsec, mtl, optparse-applicative, prettyprinter
      , scientific, stdenv, text, time
      }:
      mkDerivation {
        pname = "beans";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base bifunctors containers exceptions filepath megaparsec mtl
          optparse-applicative prettyprinter scientific text time
        ];
        homepage = "https://github.com/sboehler/beans#readme";
        description = "A plain text accounting tool";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
