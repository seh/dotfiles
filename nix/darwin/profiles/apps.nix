# Basis of inspiration:
#  https://github.com/midchildan/dotfiles/blob/7cdd097dd01e0678b6ff56487689c78469237722/nix/darwin/profiles/apps.nix
{ config, lib, ... }:

{
  options.dotfiles.profiles.apps.enable =
    lib.mkEnableOption "essential apps for Macs";

  config = lib.mkIf config.dotfiles.profiles.apps.enable {
    # TODO(seh): Adapt configuration for Homebrew.
  };
}
