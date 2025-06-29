{ config, lib, ... }:

{
  options.dotfiles.profiles.enableAll = lib.mkEnableOption "all profiles provided by the dotfiles";

  config = lib.mkIf config.dotfiles.profiles.enableAll {
    dotfiles.profiles = {
      desktop.enable = lib.mkDefault true;
      development.enable = lib.mkDefault true;
      essential.enable = lib.mkDefault true;
      # TODO(seh): Activate these as we define them.
      # extras.enable = mkDefault true;
      # debugTools.enable = mkDefault true;
    };
  };
}
