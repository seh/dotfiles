# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/8105b21f1a743960a8bbbf5bdcd752a7d9b60d10/nix/home/profiles/fonts.nix
{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (pkgs.stdenv.hostPlatform) isLinux;
in
{
  options.dotfiles.profiles.fonts.enable = lib.mkEnableOption "recommended fonts";

  config = lib.mkIf config.dotfiles.profiles.fonts.enable {
    fonts.fontconfig.enable = lib.mkDefault isLinux;

    home.packages = with pkgs; [
      # NB: The "powerline-fonts" package contains DejaVu Sans Mono,
      # Iconsolata, and Meslo. However, there are additional fonts in
      # the DejaVu family, so we'll pull them in as well.
      #
      # Excluded:
      # - inconsolata 
      # - meslo-lgs-nf
      dejavu_fonts
      jetbrains-mono
      nerdfonts
      source-code-pro
    ];
  };
}
