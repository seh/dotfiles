{
  dotfiles.featureModules.nixOS.nix = {
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
