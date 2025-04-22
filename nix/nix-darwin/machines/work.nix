# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/9d3f1a4b8d8a9203e34e331f46cc18f844829aa3/nix/darwin/machines/ci.nix#L8
{ dotfiles, ... }:

let
  username = dotfiles.lib.config.user.name;
  homeDirectory = dotfiles.lib.config.user.darwin.homeDirectory;
in
{
  users.users.${username}.home = homeDirectory;

  dotfiles.profiles.apps.enable = true;

  # See https://nix-community.github.io/home-manager/#sec-install-nixos-module.
  home-manager.users.${username} = {
    dotfiles.profiles = {
      enableAll = true;
      development = {
        enableRust = true;
      };
      # TODO(seh): Activate these as we define them.
      #extras.enable = false;
    };
  };
}
