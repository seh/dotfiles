{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.dotfiles.gnupg;
  inherit (config.dotfiles) flakeOptions;
  userConfig = flakeOptions.user;
  hasGPGSigningKey = builtins.hasAttr "gpgKey" userConfig && userConfig.gpgKey != "";
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
in
{
  options.dotfiles.gnupg = {
    enable = lib.mkEnableOption "GnuPG";

    enableSSHSupport = lib.mkEnableOption "GnuPG SSH support";
  };

  config = lib.mkIf cfg.enable {
    programs.gpg = {
      enable = true;
      settings = lib.mkIf hasGPGSigningKey {
        default-key = userConfig.gpgKey;
      };
    };

    services.gpg-agent = {
      enable = true;
      defaultCacheTtl = 28800;
      maxCacheTtl = 28800;
      enableSshSupport = cfg.enableSSHSupport;
      pinentry.package = if isDarwin then pkgs.pinentry_mac else pkgs.pinentry-curses;
      enableBashIntegration = true;
      enableFishIntegration = true;
      enableNushellIntegration = true;
      enableZshIntegration = true;
    };
  };
}
