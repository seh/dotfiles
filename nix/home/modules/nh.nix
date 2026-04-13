{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.dotfiles.nh;
in {
  options.dotfiles.nh = {
    enable = lib.mkEnableOption "nh";
  };

  config = lib.mkIf cfg.enable {
    programs.nh =
      {
        enable = lib.mkDefault true;
        package = lib.mkDefault pkgs.nh;
        clean = {
          enable = lib.mkDefault true;
          extraArgs = "--optimize";
        };
      }
      // (
        let
          hostName = "local";
        in {
          darwinFlake = lib.mkDefault (".#darwinConfigurations." + hostName);
          osFlake = lib.mkDefault (".#nixosConfigurations." + hostName);
        }
      );
  };
}
