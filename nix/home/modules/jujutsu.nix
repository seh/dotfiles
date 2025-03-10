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
      settings = lib.mkMerge [
        cfg.extraSettings
        {
          core = {
            fsmonitor = "watchman";
            watchman = {
              register_snapshot_trigger = true;
            };
          };
          git = {
            push-new-bookmarks = true;
          };
          revset-aliases = {
            "lagging_bookmarks" = ''
              ::bookmarks()
              & mutable()
              & mine()
              ~ trunk()::
            '';
          };
          template-aliases = {
            # Basis of inspiration:
            #   https://github.com/martinvonz/jj/blob/main/docs/config.md#display-of-commit-and-change-ids
            #   https://v5.chriskrycho.com/essays/jj-init/#revisions-and-revsets.
            "format_short_id(id)" = "id.shortest(12)";
            "format_short_signature(signature)" = "signature";
            "format_timestamp(timestamp)" = "timestamp.ago()";

            "boxquoted(body, title)" = ''
              surround(
                "\n,----" ++ if(title, "[ " ++ title ++ " ]\n", "\n"),
                "`----\n",
                indent(
                  "| ",
                  body))
            '';
          };
          templates = {
            log = ''
              builtin_log_compact ++
              if(!empty &&
                 (!description ||
                  description.starts_with("WIP:") ||
                  current_working_copy),
                concat(
                  boxquoted(diff.summary(), "summary"),
                  boxquoted(diff.stat(80), "stat")))
            '';
          };
          ui = {
            editor = "emacsclient";
            log-word-wrap = true;
            # See https://github.com/jj-vcs/jj/blob/main/docs/config.md#processing-contents-to-be-paged.
            pager = "delta";
            diff = {
              # See https://github.com/jj-vcs/jj/blob/main/docs/config.md#processing-contents-to-be-paged.
              format = "git";
            };
          };
        }
      ];
    };
  };
}
