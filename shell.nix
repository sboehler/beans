{
  nixpkgs ? import <nixpkgs> {},
  compiler ? "ghc844",
  withHoogle ? true
}:

let

  inherit (nixpkgs) pkgs;

  f = import ./default.nix;

  haskellPackages = pkgs.haskell.packages.${compiler};

  # Haskell IDE Engine
  hies = (import (builtins.fetchGit {
    url = "https://github.com/domenkozar/hie-nix/";
    rev = "a7ef4c4ceef1dbf46aabff68a4b9bd86d41d0886";
  }) {}).hies;

  hspkgs = (
      if withHoogle then
        haskellPackages.override {
          overrides = (self: super: {
            ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
            ghcWithPackages = self.ghc.withPackages;
          });
        }
        else haskellPackages
    );


  drv = hspkgs.callPackage f {};

  drvWithTools = pkgs.haskell.lib.addBuildDepends drv (with hspkgs; [
    hies
    ghcid
    cabal-install
    brittany
    Cabal_2_4_1_0
    hlint
    stylish-haskell
  ]);

in

  if pkgs.lib.inNixShell then drvWithTools.env else drvWithTools
