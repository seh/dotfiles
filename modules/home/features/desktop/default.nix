{flakeLib, ...}:
flakeLib.mkFeature "desktop" {
  # The desktop bundle brings along the graphical bundles and the
  # conferencing client. Naming a bundle constrained to particular
  # platforms (via its "supportedPlatforms") is safe everywhere: such
  # a bundle activates only where the host's platform qualifies.
  implies = [
    "apps"
    "desktop/zoom"
    "fonts"
    "macos"
  ];
}
