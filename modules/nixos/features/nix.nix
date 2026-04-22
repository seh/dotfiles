{
  lib,
  pkgs,
  config,
  ...
}: {
  nix.package = lib.mkDefault pkgs.lixPackageSets.${config.dotfiles.flakeOptions.lix.channel}.lix;
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];
}
