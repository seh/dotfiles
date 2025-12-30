{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.dotfiles.opencode;
in
{
  options.dotfiles.opencode = {
    enable = lib.mkEnableOption "opencode";
  };

  config = lib.mkIf cfg.enable {
    programs.opencode = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.opencode;
      # TODO(SEHarris): Consider defining these attributes:
      # rules = "";
      settings = {
        formatter = {
          gofumpt = {
            command = [
              "gofumpt"
              "$FILE"
            ];
            extensions = [ ".go" ];
          };
        };
        tui = {
          scroll_acceleration = {
            enabled = true;
          };
        };
      };
    };
  };
}
