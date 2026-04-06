{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.dotfiles.opencode;
in {
  options.dotfiles.opencode = {
    enable = lib.mkEnableOption "opencode";
  };

  config = lib.mkIf cfg.enable {
    programs.opencode = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.opencode;
      settings = {
        formatter = {
          gofumpt = {
            command = [
              "gofumpt"
              "$FILE"
            ];
            extensions = [".go"];
          };
        };
      };
      skills = lib.genAttrs [
        "catch-up-on-recent-jj-changes"
        "close-bazel-drift"
      ] (name: ./skills/${name});
      tui = {
        scroll_acceleration = {
          enabled = true;
        };
      };
    };
  };
}
