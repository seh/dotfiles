{flakeLib, ...}:
flakeLib.mkProfile "desktop" {
  # The desktop profile brings along the graphical profiles.
  # Listing profiles constrained to particular platforms (via their
  # "supportedPlatforms") is safe everywhere: such a profile activates
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
