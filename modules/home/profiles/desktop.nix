{flakeLib, ...}:
flakeLib.mkProfile "desktop" {
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
