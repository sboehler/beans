{
  nixpkgs ? import <nixpkgs> {},
  compiler ? "ghc822",
  withHoogle ? true
}:

let

  inherit (nixpkgs) pkgs;

  f = import ./default.nix;

  haskellPackages = pkgs.haskell.packages.${compiler};

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
    hspkgs.ghcid
    hspkgs.cabal-install
    hspkgs.brittany
    hspkgs.hlint
    hspkgs.stylish-haskell
  ];

in

  if pkgs.lib.inNixShell then drvWithTools.env else drvWithTools
