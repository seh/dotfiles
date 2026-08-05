{flakeLib, ...}:
# The Zoom conferencing client, installed on the platforms where
# nixpkgs offers it.
flakeLib.mkFeature "desktop/zoom" {
  homeManager = {
    lib,
    pkgs,
    ...
  }: {
    home.packages = let
      candidatePkg = pkgs.zoom-us;
    in
      lib.optionals (lib.meta.availableOn pkgs.stdenv.hostPlatform candidatePkg) [
        candidatePkg
      ];
  };
}
