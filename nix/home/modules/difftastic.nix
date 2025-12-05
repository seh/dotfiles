{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.dotfiles.difftastic;
in
{
  options.dotfiles.difftastic = {
    enable = lib.mkEnableOption "difftastic";
  };

  config = lib.mkIf cfg.enable {
    programs.difftastic = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.difftastic;
      git = {
        enable = true;
      };
    };
  };
}
