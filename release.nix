let
  pkgs = import <nixpkgs> {};
in {
  beans = pkgs.haskellPackages.callPackage ./default.nix {};
}
