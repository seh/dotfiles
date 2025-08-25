{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.dotfiles.gemini;
in
{
  options.dotfiles.gemini = {
    enable = lib.mkEnableOption "gemini";
  };

  config = lib.mkIf cfg.enable {
    programs.gemini-cli = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.gemini-cli;
      settings = {
        checkpointing = {
          enabled = true;
        };
        contextFileName = [
          "CRUSH.md"
          "GEMINI.md"
        ];
        disableUpdateNag = true;
        selectedAuthType = "vertex-ai";
        theme = "Atom One";
      };
    };
  };
}
