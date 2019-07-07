{
  config ? {},
  overlays ? []
}:
let
  sources = import ./sources.nix;
in
import sources.nixpkgs
  { overlays =
      [
        (_: pkgs: { inherit sources; })
        (_: pkgs: {
          niv = (import sources.niv {}).niv;
        })
      ] ++ overlays;
    inherit config;
  }
