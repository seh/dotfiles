# Basis of inspiration:
#  https://github.com/midchildan/dotfiles/blob/7cdd097dd01e0678b6ff56487689c78469237722/nix/darwin/profiles/apps.nix
{ config, lib, ... }:

{
  options.dotfiles.profiles.apps.enable =
    lib.mkEnableOption "essential apps for Macs";

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
        "homebrew/cask"
      ];

      casks = [
        # TODO(seh): Once the GUI becomes viable to install via Nix,
        # remove this in favor of that.
        "1password"
        "hammerspoon"
        "slack"
      ];

      brews = [
      ];
    };
  };
}
