{
  compiler ? "ghc864",
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
    rev = "6794005f909600679d0b7894d0e7140985920775";
  }) {}).hies;

  drv = haskellPackages.callPackage f {};

  drvWithTools = pkgs.haskell.lib.addBuildDepends drv (
    with haskellPackages; [
        hies
        ghcid
        cabal-install
        hlint
        hindent
        stylish-haskell
      ]
    ++ (with pkgs.nodePackages; [pulp])
    ++ (with pkgs; [purescript psc-package]));
in

  if pkgs.lib.inNixShell then drvWithTools.env else drvWithTools
