{ config, lib, ... }:

with lib;

{
  imports = [
    ./desktop.nix
    ./development.nix
    ./essential.nix
    ./fonts.nix
    ./macos.nix
    ./minimal.nix
    ./web.nix
  ];

  options.dotfiles.profiles.enableAll = mkEnableOption "all profiles provided by the dotfiles";

  config = mkIf config.dotfiles.profiles.enableAll {
    dotfiles.profiles = {
      desktop.enable = mkDefault true;
      development.enable = mkDefault true;
      essential.enable = mkDefault true;
      # TODO(seh): Activate these as we define them.
      # extras.enable = mkDefault true;
      # debugTools.enable = mkDefault true;
    };
  };
}
