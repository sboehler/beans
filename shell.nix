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
  hies = (import (builtins.fetchTarball {
    url = "https://github.com/domenkozar/hie-nix/tarball/master";
    sha256 = "0hilxgmh5aaxg37cbdwixwnnripvjqxbvi8cjzqrk7rpfafv352q";
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

  drvWithTools = pkgs.haskell.lib.addBuildDepends drv [
    hies
    hspkgs.ghcid
    hspkgs.cabal-install
    hspkgs.brittany
    hspkgs.hlint
    hspkgs.stylish-haskell
  ];

in

  if pkgs.lib.inNixShell then drvWithTools.env else drvWithTools
