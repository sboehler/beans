let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
in {
  beans = pkgs.haskellPackages.callPackage ./default.nix {};
}
