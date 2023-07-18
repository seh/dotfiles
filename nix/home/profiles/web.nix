# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/152b40c3a412b18ba6057c3ecfb984748962282b/nix/home/profiles/web.nix
{ config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv.hostPlatform) isDarwin isLinux;
in
{
  options.dotfiles.profiles.web.enable =
    lib.mkEnableOption "opinionated defaults for Web browsers";

  config = lib.mkIf config.dotfiles.profiles.web.enable {
    dotfiles.firefox = {
      enable = lib.mkDefault true;
      # TODO(seh): Set preferences.

      policies.Preferences."browser.contentblocking.category" = {
        Value = lib.mkDefault "strict";

        # Firefox forcibly sets this option to "custom" if:
        #   1. The setting doesn't appear to be set by the user
        #   2. Related settings deviate from the expected values
        # https://searchfox.org/mozilla-central/rev/201b2c1/browser/components/BrowserGlue.jsm#5059
        Status = lib.mkDefault "user";
      };
    };

    targets.darwin = lib.mkIf isDarwin {
      defaults."com.apple.Safari" = {
        # TODO(seh): Include Safari customizations.
      };
    };
  };
}
