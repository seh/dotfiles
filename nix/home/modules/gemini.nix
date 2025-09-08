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
          "AGENTS.md"
          "GEMINI.md"
        ];
        disableUpdateNag = true;
        preferredEditor = "emacs";
        selectedAuthType = "vertex-ai";
        showLineNumbers = true;
        showMemoryUsage = true;
        theme = "Atom One";
      };
    };
  };
}
