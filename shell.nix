let
  sources = import ./nix/sources.nix;

  config = {
    # allowBroken = true;
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: {
          ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
          ghcWithPackages = self.ghc.withPackages;
        };
      };
    };
  };

  pkgs = import sources.nixpkgs { inherit config; };

  drv = pkgs.haskellPackages.callPackage (import ./default.nix) {};

  drvWithTools = pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
    cabal-install
    cabal2nix
    dhall
    ghcid
    ghcide
    hlint
    hoogle
    ormolu
    stylish-haskell
  ]);

in

  if pkgs.lib.inNixShell then drvWithTools.env else drvWithTools
