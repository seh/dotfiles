{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.dotfiles.helix;
in
{
  options.dotfiles.helix = {
    enable = lib.mkEnableOption "helix";
  };

  config = lib.mkIf cfg.enable {
    programs.helix = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.helix;
      # TODO(SEHarris): Consider defining these attributes:
      # ignores = []
      # languages = {}
      # settings = {}
    };
  };
}
