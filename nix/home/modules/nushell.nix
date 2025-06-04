{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.dotfiles.nushell;
in
{
  options.dotfiles.nushell = {
    enable = lib.mkEnableOption "nushell";
  };

  config = lib.mkIf cfg.enable {
    programs.nushell = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.nushell;
      plugins = with pkgs.nushellPlugins; [
        formats
        highlight
        net
        polars
        query
        #semver # TODO(seh): This one is not available for Darwin atop x86-64 for now.
        units
      ];
    };
    home.packages = with pkgs; [
      nufmt
    ];
  };
}
