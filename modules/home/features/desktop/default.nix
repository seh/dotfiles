{flakeLib, ...}:
flakeLib.mkFeature "desktop" {
  # The desktop bundle implies the graphical bundles and the
  # conferencing client. Listing a bundle constrained to particular
  # platforms (via its "supportedPlatforms") is safe everywhere: such
  # a bundle activates only where the host's platform qualifies.
  implies = [
    "apps"
    "desktop/zoom"
    "fonts"
    "macos"
  ];
}
