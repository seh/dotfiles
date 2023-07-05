{ config, lib, pkgs, ... }:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
in
{
  options.dotfiles.profiles.desktop.enable =
    lib.mkEnableOption "essential packages for desktop environemnts";

  config = lib.mkIf config.dotfiles.profiles.desktop.enable {
    dotfiles.emacs.enable = lib.mkDefault true;
    dotfiles.profiles = {
      macos.enable = lib.mkDefault isDarwin;
      # TODO(seh): Enable other profiles here.
    };
  };
}
