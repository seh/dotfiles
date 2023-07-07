{ lib, config, ... }:

let
  cfg = config.dotfiles.hammerspoon;
in
{
  options.dotfiles.hammerspoon.enable = lib.mkEnableOption "Hammerspoon";

  config = lib.mkIf cfg.enable {
    home.file.".hammerspoon/init.lua".source = ./init.lua;
  };

  # NB: For now there is no Nix package for Hammerspoon. See
  # https://github.com/NixOS/nixpkgs/issues/146460 for the extant
  # request for its inclusion.
}
