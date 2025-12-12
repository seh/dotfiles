{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
in
{
  options.dotfiles.profiles.desktop.enable =
    lib.mkEnableOption "essential packages for desktop environemnts";

  config = lib.mkIf config.dotfiles.profiles.desktop.enable {
    home.packages =
      let
        candidatePkg = pkgs.zoom-us;
      in
      lib.optionals (lib.meta.availableOn pkgs.stdenv.hostPlatform candidatePkg) [
        candidatePkg
      ];

    dotfiles.emacs.enable = lib.mkDefault true;
    dotfiles.profiles = {
      fonts.enable = lib.mkDefault true;
      macos.enable = lib.mkDefault isDarwin;
    };
  };
}
