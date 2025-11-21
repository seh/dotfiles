# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/98f983380770d6f6d33a828f41f3656adeb4e9a7/nix/darwin/modules/nix.nix
{
  lib,
  pkgs,
  config,
  ...
}:

let
  inherit (config.dotfiles) flakeOptions;
  nixAttrName = flakeOptions.nix.package;
in
{
  nix.package = lib.mkDefault pkgs.nixVersions.${nixAttrName};
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';
}
