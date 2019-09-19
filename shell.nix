let
  sources = import ./nix/sources.nix;

  pkgs = import sources.nixpkgs {};

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
      # ghcWithPackages = self.ghc.withPackages;
      ormolu = self.callCabal2nix "ormolu" sources.ormolu {};
    };
  };

  drv = haskellPackages.callPackage (import ./default.nix) {};

  all-hies = (import sources.all-hies {}).selection {
    selector = p: { inherit (p) ghc865; };
  };

  ghcide = (import (builtins.fetchTarball "https://github.com/hercules-ci/ghcide-nix/tarball/master") {}).ghcide-ghc865;

  drvWithTools = pkgs.haskell.lib.addBuildDepends drv (
    with haskellPackages; [
      ghcid
      cabal-install
      hlint
      hasktags
      hindent
      stylish-haskell
      ormolu
      beam-core
    ]
    ++ (with pkgs.nodePackages; [pulp])
    ++ (with pkgs; [purescript psc-package])
    ++ [ghcide pkgs.cacert]
  );
in
if pkgs.lib.inNixShell then drvWithTools.env else drvWithTools
