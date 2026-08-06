{flakeLib, ...}:
flakeLib.mkFeature "model-agent/copilot" {
  unfreePackages = ["github-copilot-cli"];

  homeManager = {
    lib,
    pkgs,
    ...
  }: {
    programs.github-copilot-cli = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.github-copilot-cli;
    };
  };
}
