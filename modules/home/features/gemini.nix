{flakeLib, ...}:
flakeLib.mkFeature "gemini" {
  homeManager = {
    lib,
    pkgs,
    ...
  }: {
    programs.gemini-cli = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.gemini-cli;
      settings = {
        context = {
          fileName = [
            "AGENTS"
            "GEMINI.md"
          ];
        };
        general = {
          checkpointing = {
            enabled = true;
          };
          disableUpdateNag = true;
          preferredEditor = "emacs";
        };
        security = {
          auth = {
            selectedType = "vertex-ai";
          };
        };
        ui = {
          showCitations = true;
          showMemoryUsage = true;
          showLineNumbers = true;
          theme = "Atom One";
        };
      };
    };
  };
}
