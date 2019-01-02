{
  nixpkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/44b02b52ea6a49674f124f50009299f192ed78bb.tar.gz";
    sha256 = "0gmk6w1lnp6kjf26ak8jzj0h2qrnk7bin54gq68w1ky2pdijnc44";
  }) {},
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
