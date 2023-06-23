# Basis of inspiration:
#  https://github.com/sebastiant/dotfiles/blob/master/hosts/macbook/home.nix
{ pkgs, ... }:
{
  imports = [
    ../common.nix
    ../../programs/allow-non-free.nix
  ];
  home.packages = with pkgs; [
    # TODO(seh): Consider populating these.
  ];
  # TODO(seh): Adapt more of that.

  # NB: Work around the following defect temporarily:
  # https://github.com/nix-community/home-manager/issues/4142
  manual.manpages.enable = false;
}
