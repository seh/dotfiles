# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/98f983380770d6f6d33a828f41f3656adeb4e9a7/nix/darwin/modules/nix.nix
{flakeLib, ...}:
flakeLib.mkFeature "nix" {
  nixDarwin = {
    lib,
    pkgs,
    config,
    ...
  }: {
    nix.package = lib.mkDefault pkgs.lixPackageSets.${config.dotfiles.lix.channel}.lix;
    nix.settings.experimental-features = [
      "nix-command"
      "flakes"
    ];
  };
}
