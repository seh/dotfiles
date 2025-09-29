{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.dotfiles.claude;
in
{
  options.dotfiles.claude = {
    enable = lib.mkEnableOption "claude";
  };

  config = lib.mkIf cfg.enable {
    programs.claude-code = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.claude-code;
      settings = {
        includeCoAuthoredBy = false;
        model = "opusplan";
      };
    };
  };
}
