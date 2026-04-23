{
  dotfiles.featureModules.homeManager.helix = {
    config,
    lib,
    pkgs,
    ...
  }: let
    cfg = config.dotfiles.helix;
  in {
    options.dotfiles.helix = {
      enable = lib.mkEnableOption "helix";
    };

    config = lib.mkIf cfg.enable {
      programs.helix = {
        enable = lib.mkDefault true;
        package = lib.mkDefault pkgs.helix;
        # TODO(seh): Consider defining these attributes:
        # ignores = []
        # languages = {}
        # settings = {}
      };
    };
  };
}
