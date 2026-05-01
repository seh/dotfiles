{flakeLib, ...}:
flakeLib.mkFeature "nix" {
  nixOS = {
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
