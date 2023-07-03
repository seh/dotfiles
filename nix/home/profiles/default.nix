{ config, lib, ... }:

with lib;

{
  imports = [
    ./essential.nix
    # TODO(seh): Include other profile modules.
  ];

  options.dotfiles.profiles.enableAll =
    mkEnableOption "all profiles provided by the dotfiles";

  config = mkIf config.dotfiles.profiles.enableAll {
    dotfiles.profiles = {
      essential.enable = mkDefault true;
      # TODO(seh): Activate these as we define them.
      # extras.enable = mkDefault true;
      # debugTools.enable = mkDefault true;
      # desktop.enable = mkDefault true;
      # development.enable = mkDefault true;
    };
  };  
}
