{flakeLib, ...}:
flakeLib.mkFeature "nix" {
  nixOS = {
    lib,
    pkgs,
    config,
    ...
  }: {
    nix.package = lib.mkDefault pkgs.lixPackageSets.${config.dotfiles.lix.channel}.lix;
    nix.settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      # NB: NixOS includes the "root" user automatically.
      trusted-users = builtins.attrNames config.dotfiles.users;
    };
  };
}
