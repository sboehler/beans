{
  # A nixos stable snapshot for tools that don't build with GHC 8.6
  stable ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/44b02b52ea6a49674f124f50009299f192ed78bb.tar.gz";
    sha256 = "0gmk6w1lnp6kjf26ak8jzj0h2qrnk7bin54gq68w1ky2pdijnc44";
  }) {},

  # My system wide nix snapshot, usually tracking nixos-unstable
  unstable ? import <nixpkgs> {},
  compiler ? "ghc863",
  withHoogle ? true
}:

let

  inherit (unstable) pkgs;

  f = import ./default.nix;

  haskellPackages = pkgs.haskell.packages.${compiler};

  # Haskell IDE Engine
  #
  # When upgrading, make sure to delete ~/.cache/cabal-helper!
  #
  hies = (import (builtins.fetchGit {
    url = "https://github.com/domenkozar/hie-nix/";
    rev = "19f47e0bf2e2f1a793bf87d64bf8266062f422b1";
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
    hlint
    stable.haskellPackages.brittany
    stylish-haskell
  ]);

in

  if pkgs.lib.inNixShell then drvWithTools.env else drvWithTools
