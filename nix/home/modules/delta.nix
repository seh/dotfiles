{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.dotfiles.delta;
in
{
  options.dotfiles.delta = {
    enable = lib.mkEnableOption "delta";
  };

  config = lib.mkIf cfg.enable {
    programs.delta = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.delta;
      enableGitIntegration = true;
      options = {
        features = "navigate";
      };
    };
  };
}
