let
  sources = import ./nix/sources.nix;

  # config = {
  #   packageOverrides = pkgs: rec {
  #     haskell = pkgs.haskell // {
  #       packages = pkgs.haskell.packages // {
  #         ${compiler} = pkgs.haskell.packages.${compiler}.override {
  #           overrides = self: super: {
  #             ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
  #             ghcWithPackages = self.ghc.withPackages;
  #           };
  #         };
  #       };
  #     };
  #   };
  # };

  pkgs = import sources.nixpkgs {};

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages = self.ghc.withPackages;
    };
  };

  drv = haskellPackages.callPackage (import ./default.nix) {};

  all-hies = (import sources.all-hies {}).selection {
    selector = p: { inherit (p) ghc865; };
  };

  ghcide = (import (builtins.fetchTarball "https://github.com/hercules-ci/ghcide-nix/tarball/master") {}).ghcide-ghc865;

  drvWithTools = pkgs.haskell.lib.addBuildDepends drv (
    with haskellPackages; [
      ghcid.bin
      cabal-install
      hlint
      hasktags
      hindent
      stylish-haskell
    ]
    ++ (with pkgs.nodePackages; [pulp])
    ++ (with pkgs; [purescript psc-package docker-compose sqlite ormolu])
    ++ [pkgs.cacert]
  );
in
if pkgs.lib.inNixShell then drvWithTools.env else drvWithTools
