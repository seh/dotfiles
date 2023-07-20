# nix-darwin configuration
#
# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/98f983380770d6f6d33a828f41f3656adeb4e9a7/nix/darwin/modules/shell.nix
{ config, lib, pkgs, ... }:

{
  # Create /etc/zshrc that loads the nix-darwin environment.
  # See https://github.com/LnL7/nix-darwin/issues/177.
  programs.zsh.enable = true;

  environment.systemPackages = with pkgs;
    [
      docker
    ];

  # TODO(seh): Consider adapting more of the example configuration,
  # such as environment variables and system defaults.
}
