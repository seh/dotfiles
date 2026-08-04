{flakeLib, ...}:
flakeLib.mkFeature "desktop" {
  # The desktop bundle brings along the graphical bundles. Naming a
  # bundle constrained to particular platforms (via its
  # "supportedPlatforms") is safe everywhere: such a bundle activates
  # only where the host's platform qualifies.
  implies = [
    "apps"
    "fonts"
    "macos"
  ];

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
