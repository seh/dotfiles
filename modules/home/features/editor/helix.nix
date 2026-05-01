{flakeLib, ...}:
flakeLib.mkFeature "editor/helix" {
  homeManager = {
    lib,
    pkgs,
    ...
  }: {
    programs.helix = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.helix;
      # TODO(seh): Consider defining these attributes:
      # ignores = []
      # languages = {}
      # settings = {}
    };
  };
}
