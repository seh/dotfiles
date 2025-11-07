{ config, ... }:

let
  username = config.dotfiles.flakeOptions.user.name;
in
{
  users.users.${username}.home = "/Users/${username}";

  dotfiles.profiles.apps.enable = true;

  # See https://nix-community.github.io/home-manager/#sec-install-nixos-module.
  home-manager.users.${username} = {
    dotfiles.profiles = {
      enableAll = true;
      # TODO(seh): Activate these as we define them.
      #extras.enable = false;
    };
    home.stateVersion = "25.11";
  };

  system.stateVersion = 6;
}
