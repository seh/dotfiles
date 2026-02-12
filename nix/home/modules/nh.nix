{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.dotfiles.nh;
in
{
  options.dotfiles.nh = {
    enable = lib.mkEnableOption "nh";
  };

  config = lib.mkIf cfg.enable {
    programs.nh = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.nh;
      clean = {
        enable = lib.mkDefault true;
        extraArgs = "--optimize";
      };
      # TODO(seh): At present, with the "NH_DARWIN_FLAKE" environment
      # variable set, the "nh darwin build ." can't find a suitable
      # target. Without that variable set, though, it does, likely
      # matching by hostname instead.
      #darwinFlake = lib.mkDefault ".#local";
    };
  };
}
