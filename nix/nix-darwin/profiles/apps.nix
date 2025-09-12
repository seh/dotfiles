# Basis of inspiration:
#  https://github.com/midchildan/dotfiles/blob/7cdd097dd01e0678b6ff56487689c78469237722/nix/darwin/profiles/apps.nix
{
  config,
  lib,
  ...
}:

{
  options.dotfiles.profiles.apps.enable = lib.mkEnableOption "essential apps for Macs";

  config = lib.mkIf config.dotfiles.profiles.apps.enable {
    # Manage Homebrew with nix-darwin. Mainly useful for managing casks and
    # App Store installations. Any formula or cask not specified in the config
    # would be automatically uninstalled by default.
    #
    # See
    # https://daiderd.com/nix-darwin/manual/index.html#opt-homebrew.enable
    # for the pertinent attributes.
    homebrew = {
      enable = lib.mkDefault true;
      onActivation = {
        autoUpdate = lib.mkDefault true;
        upgrade = lib.mkDefault true;
        cleanup = lib.mkDefault "uninstall";
      };

      taps = [
        # NB: As of late 2023, it's no longer necessary to tap
        # "homebrew/cask".
      ];

      casks = [
        # TODO(seh): Once the GUI becomes viable to install via Nix,
        # remove this in favor of that.
        "1password"
        # NB: Use the Homebrew cask-installed application since the
        # Nix package can't arrange for the daemon to run on macOS for
        # now, per https://github.com/LnL7/nix-darwin/issues/112.
        "docker-desktop"
        # NB: We can't make this contingent on the Home Manager
        # configuration that enables Firefox, because we can't see
        # that attribute from here.
        "firefox"
        "linearmouse"
        "slack"
      ];

      brews = [ ];
    };
  };
}
