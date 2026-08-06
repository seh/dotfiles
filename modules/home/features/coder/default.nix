{flakeLib, ...}:
flakeLib.mkFeature "coder" {
  # The nixpkgs "coder" package is itself free, while the wrapper it
  # installs puts the "terraform" executable on the program's path, so
  # instantiating it demands toleration for that.
  unfreePackages = ["terraform"];

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
      dotfiles.coder.enableSSHIntegration = lib.mkDefault true;

      home.packages = [
        cfg.package
      ];
      programs.ssh.settings =
        lib.mkIf cfg.enableSSHIntegration
        (flakeLib.onlyWhen config ["ssh"] {
          "coder.*.main" = {
            ConnectTimeout = "0";
            LogLevel = "ERROR";
            ProxyCommand = "${lib.getExe cfg.package} ssh --stdio --ssh-host-prefix 'coder.' %h";
            UserKnownHostsFile = "/dev/null";
          };
        });
    };
  };
}
