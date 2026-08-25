# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/152b40c3a412b18ba6057c3ecfb984748962282b/nix/home/profiles/web.nix
{flakeLib, ...}:
flakeLib.mkFeature "web" {
  # The web bundle implies the Firefox customization feature.
  implies = ["web/firefox"];
}
