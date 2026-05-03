{flakeLib, ...}:
flakeLib.mkFeature "model-agent/copilot" {
  homeManager = {
    lib,
    pkgs,
    ...
  }: {
    programs.github-copilot-cli = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.copilot-cli;
    };
  };
}
