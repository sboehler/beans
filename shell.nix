let
  sources = import ./nix/sources.nix;

  overlays = [
    (_: pkgs: {
      haskell = pkgs.haskell // {
        packageOverrides = self: super: {
          ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
          ghcWithPackages = self.ghc.withPackages;
          ormolu = self.callCabal2nix "ormolu" sources.ormolu {};
        };
      };
    })
  ];

  pkgs = import sources.nixpkgs {
    overlays = overlays;
  };

  drv = pkgs.haskellPackages.callPackage (import ./default.nix) {};

  drvWithTools = pkgs.haskell.lib.addBuildDepends drv (
    with pkgs.haskellPackages; [
      ghcid
      cabal-install
      hlint
      hindent
      stylish-haskell
      ormolu
    ]
    ++ (with pkgs.nodePackages; [pulp])
    ++ (with pkgs; [purescript psc-package])
  );
in
if pkgs.lib.inNixShell then drvWithTools.env else drvWithTools
