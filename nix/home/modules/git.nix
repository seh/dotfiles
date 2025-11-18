{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.dotfiles) flakeOptions;
  userConfig = flakeOptions.user;
  hasGPGSigningKey = builtins.hasAttr "gpgKey" userConfig && userConfig.gpgKey != "";
  cfg = config.dotfiles.git;
in
{
  options.dotfiles.git = {
    enable = lib.mkEnableOption "git";
  };

  config = lib.mkIf cfg.enable {
    programs.git = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.git;

      ignores = [
        "**/.claude/settings.local.json"
        "*~"
        ".#*"
      ];
      lfs.enable = lib.mkDefault true;
      settings = {
        aliases = {
          # See https://ses4j.github.io/2020/04/01/git-alias-recent-branches/.
          # (And https://ses4j.github.io/2020/04/01/git-alias-recent-branches/#comment-4863945965 for the right-aligned column.)
          lb = ''
            !git reflog show --pretty=format:'%gs ~ %gd' --date=relative | grep 'checkout:' | grep -oE '[^ ]+ ~ .*' | awk -F~ '!seen[''$1]++' | head -n 10 | awk -F' ~ HEAD@{' '{printf("  \033[33m%12s:\t\033[37m %s\033[0m\n", substr(''$2, 1, length(''$2)-1), ''$1)}'
          '';
          rb = ''
            for-each-ref --sort='-authordate:iso8601' --format=' %(align:25)%(color:green)%(authordate:relative)%(end)%(color:bold blue)%(refname:short)' refs/heads
          '';
        };
        branch = {
          autoSetupMerge = "always";
          autoSetupRebase = "local";
        };
        merge = {
          conflictStyle = "zdiff3";
        };
        rebase = {
          autosqaush = true;
        };
        rerere = {
          enabled = 1;
          autoupdate = 1;
        };
        # Per https://golang.org/doc/faq#git_https, for gopls against private repositories:
        url = {
          "ssh://git@github.com/" = {
            insteadOf = "https://github.com/";
          };
        };
        user = {
          inherit (userConfig) email;
          name = userConfig.fullName;
        };
      }
      # TODO(seh): Specify "includes"?
      // lib.optionalAttrs hasGPGSigningKey {
        signing = {
          key = userConfig.gpgKey;
          signByDefault = true;
        };
      };
    };
  };
}
