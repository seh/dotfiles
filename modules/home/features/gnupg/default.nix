{flakeLib, ...}:
flakeLib.mkFeature "gnupg" {
  homeManager = {
    options = {lib, ...}: {
      options.dotfiles.gnupg = {
        enableSSHSupport = lib.mkEnableOption "GnuPG SSH support";
      };
    };

    config = {
      config,
      lib,
      pkgs,
      ...
    }: let
      cfg = config.dotfiles.gnupg;
      userConfig = config.dotfiles.identity;
      hasGPGSigningKey = userConfig.gpgKey != null;
      inherit (pkgs.stdenv.hostPlatform) isDarwin;
    in {
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
        pinentry.package =
          if isDarwin
          then pkgs.pinentry_mac
          else pkgs.pinentry-curses;
        enableBashIntegration = true;
        enableFishIntegration = true;
        enableNushellIntegration = true;
        enableZshIntegration = true;
      };
    };
  };
}
