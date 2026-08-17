{flakeLib, ...}:
flakeLib.mkFeature "coder" {
  homeManager = {
    options = {
      lib,
      pkgs,
      ...
    }: {
      options.dotfiles.coder = {
        package = lib.mkPackageOption pkgs "coder" {};

        enableSSHIntegration = lib.mkEnableOption "SSH support";
      };
    };

    config = {
      config,
      lib,
      ...
    }: let
      cfg = config.dotfiles.coder;
    in {
      home.packages = [
        cfg.package
      ];
      programs.ssh.settings =
        lib.mkIf (cfg.enableSSHIntegration && config.dotfiles._host.activatesFeature "ssh")
        {
          "coder.*.main" = {
            ConnectTimeout = "0";
            LogLevel = "ERROR";
            ProxyCommand = "${lib.getExe cfg.package} ssh --stdio --ssh-host-prefix 'coder.' %h";
            UserKnownHostsFile = "/dev/null";
          };
        };
    };
  };
}
