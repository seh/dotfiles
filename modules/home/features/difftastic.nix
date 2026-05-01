{flakeLib, ...}:
flakeLib.mkFeature "difftastic" {
  homeManager = {
    lib,
    pkgs,
    ...
  }: {
    programs.difftastic = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.difftastic;
      git = {
        enable = true;
      };
    };
  };
}
