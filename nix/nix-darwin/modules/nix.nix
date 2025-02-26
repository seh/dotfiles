# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/98f983380770d6f6d33a828f41f3656adeb4e9a7/nix/darwin/modules/nix.nix
{
  lib,
  pkgs,
  dotfiles,
  ...
}:

let
  nix = dotfiles.lib.config.nix.package;
in
{
  nix.package = lib.mkDefault pkgs.nixVersions.${nix};
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';
  #nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreePredicate =
    pkg:
    builtins.elem (lib.getName pkg) [
      "1password"
      "1password-cli"
      #"discord"
      "dropbox"
      "slack"
      "zoom"
    ];
}
