let
  compiler = "ghc864";

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
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

  f = import ./default.nix;

  haskellPackages = pkgs.haskell.packages.${compiler};

  # Haskell IDE Engine
  #
  # When upgrading, make sure to delete ~/.cache/cabal-helper!
  #
  hies = (import (builtins.fetchGit {
    url = "https://github.com/domenkozar/hie-nix/";
    rev = "3568848019da43c4581e931fcb7a6cb8b0f33079";
  }) {}).hies;

  drv = haskellPackages.callPackage f {};

  drvWithTools = pkgs.haskell.lib.addBuildDepends drv (with haskellPackages; [
    hies
    ghcid
    cabal-install
    hasktags
    hlint
    stylish-haskell
  ]);

in

  if pkgs.lib.inNixShell then drvWithTools.env else drvWithTools
