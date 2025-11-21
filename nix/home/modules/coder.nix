{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.dotfiles.coder;
in
{
  options.dotfiles.coder = {
    enable = lib.mkEnableOption "coder";
    package = lib.mkPackageOption pkgs "coder" { };

    enableSSHIntegration = lib.mkEnableOption "SSH support";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      cfg.package
    ];
    programs.ssh.matchBlocks = lib.mkIf (cfg.enableSSHIntegration && config.dotfiles.ssh.enable) {
      "coder.*.main" = {
        proxyCommand = "${lib.getExe cfg.package} ssh --stdio --ssh-host-prefix 'coder.' %h";
        userKnownHostsFile = "/dev/null";
        extraOptions = {
          connectTimeout = "0";
          logLevel = "ERROR";
        };
      };
    };
  };
}
