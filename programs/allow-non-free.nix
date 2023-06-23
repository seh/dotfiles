# Basis of inspiration:
#  https://github.com/sebastiant/dotfiles/blob/master/programs/non-free.nix
{ lib, ... }:
{
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "1password"
    "1password-cli"
    #"discord"
    "dropbox"
    "slack"
    "zoom"
  ];
}
