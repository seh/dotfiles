# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/7cdd097dd01e0678b6ff56487689c78469237722/nix/home/modules/git.nix
{
  lib,
  pkgs,
  config,
  ...
}:

let
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
      extraConfig = {
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
      };
      # TODO(seh): Specify "includes"?
    };
  };
}
