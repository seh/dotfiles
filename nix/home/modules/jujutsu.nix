{
  lib,
  pkgs,
  config,
  ...
}:

let
  cfg = config.dotfiles.jujutsu;
  tomlFormat = pkgs.formats.toml { };
in
{
  options.dotfiles.jujutsu = {
    enable = lib.mkEnableOption "jujutsu";
    extraSettings = lib.mkOption {
      type = tomlFormat.type;
      default = { };
      description = "Additional settings to add to jujutsu's configuration file";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.jujutsu = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.jujutsu;
      # See https://github.com/martinvonz/jj/blob/main/docs/config.md#configuration.
      settings = cfg.extraSettings // {
        # TODO(seh): Force any overriding settings into play here.
      };
    };
  };
}
