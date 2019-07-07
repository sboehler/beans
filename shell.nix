let
  overlays = [
    (_: super: {
      haskell = super.haskell // {
        packageOverrides = self: super: {
          ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
          ghcWithPackages = self.ghc.withPackages;
        };
      };
    })
  ];

  pkgs = import ./nix {
    overlays = overlays;
  };

  drv = pkgs.haskellPackages.callPackage (import ./.) {};

  drvWithTools = pkgs.haskell.lib.addBuildDepends drv (
    with pkgs.haskellPackages; [
      ghcid
      cabal-install
      hlint
      hindent
      stylish-haskell
    ]
    ++ (with pkgs.nodePackages; [pulp])
    ++ (with pkgs; [niv purescript psc-package])
  );
in
if pkgs.lib.inNixShell then drvWithTools.env else drvWithTools
