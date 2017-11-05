{
  compiler ? "ghc863",
}:

let
  config = {

    packageOverrides = pkgs: rec {

      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
        ${compiler} = pkgs.haskell.packages.${compiler}.override {
            overrides = self: super: {
              ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
              ghcWithPackages = self.ghc.withPackages;
            };
          };
        };
      };
      profiledHaskellPackages = pkgs.haskell.${compiler}.override {
        overrides = self: super: {
          mkDerivation = args: super.mkDerivation (args // {
            enableLibraryProfiling = false;
          });
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

  f = import ./default.nix;

  # haskellPackages = pkgs.profiledHaskellPackages; # pkgs.haskell.packages.${compiler};
  haskellPackages = pkgs.haskell.packages.${compiler};

  # Haskell IDE Engine
  #
  # When upgrading, make sure to delete ~/.cache/cabal-helper!
  #
  hies = (import (builtins.fetchGit {
    url = "https://github.com/domenkozar/hie-nix/";
    rev = "19f47e0bf2e2f1a793bf87d64bf8266062f422b1";
  }) {}).hies;

  easyPS = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "0debbe629de2c2d6278ba772e40a0851a57b9d2f";
    sha256 = "1k83gsfgg4p4c2f6ls467fm8v1ixmy1y3jly8id5wc71zc7szs1q";
  });

  drv = haskellPackages.callPackage f {};

  drvWithTools = pkgs.haskell.lib.addBuildDepends drv (with haskellPackages; [
    hies
    ghcid
    cabal-install
    hlint
    hindent
    stylish-haskell] ++ ([pkgs.nodePackages.pulp pkgs.nodePackages.bower])
    ++
    (builtins.attrValues ({ inherit (easyPS.inputs) purs psc-package-simple purp; })));

in

  if pkgs.lib.inNixShell then drvWithTools.env else drvWithTools
