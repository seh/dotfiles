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
        template-aliases = {
          # Basis of inspiration:
          #   https://github.com/martinvonz/jj/blob/main/docs/config.md#display-of-commit-and-change-ids
          #   https://v5.chriskrycho.com/essays/jj-init/#revisions-and-revsets.
          "format_short_id(id)" = "id.shortest()";
          "format_short_signature(signature)" = "signature";
          "format_timestamp(timestamp)" = "timestamp.ago()";
        };
        ui = {
          log-word-wrap = true;
        };
      };
    };
  };
}
