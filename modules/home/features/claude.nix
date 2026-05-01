{flakeLib, ...}:
flakeLib.mkFeature "claude" {
  homeManager = {
    lib,
    pkgs,
    ...
  }: {
    programs.claude-code = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.claude-code;
      settings = {
        includeCoAuthoredBy = false;
      };
    };
  };
}
