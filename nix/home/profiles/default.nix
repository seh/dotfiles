{ config, lib, ... }:

with lib;

{
  imports = [
    # TODO(seh): Include other profile modules.
    ./desktop.nix
    ./development.nix
    ./essential.nix
    ./macos.nix
  ];

  options.dotfiles.profiles.enableAll =
    mkEnableOption "all profiles provided by the dotfiles";

  config = mkIf config.dotfiles.profiles.enableAll {
    dotfiles.profiles = {
      desktop.enable = mkDefault true;
      development.enable = mkDefault true;
      development.enableKubernetes = mkDefault true;
      essential.enable = mkDefault true;
      # TODO(seh): Activate these as we define them.
      # extras.enable = mkDefault true;
      # debugTools.enable = mkDefault true;
    };
  };
}
