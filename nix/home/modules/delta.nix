{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.dotfiles.delta;
in
{
  options.dotfiles.delta = {
    enable = lib.mkEnableOption "delta";
  };

  config = lib.mkIf cfg.enable {
    programs.delta = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.delta;
      enableGitIntegration = true;
      # NB: Rather than using the "delta" tool solely as a pager for
      # Git-style diffs, have Jujutsu invoke "delta" to prepare the
      # diff from the competing source files itself.
      enableJujutsuIntegration = false;
      options = {
        # Delta "Features", or named groups of options that we can
        # activate together.
        # Basis of inspiration: https://github.com/idursun/jjui/issues/314#issuecomment-3367379291
        non-split-view = {
          side-by-side = false;
        };
        split-view = {
          side-by-side = true;
        };
        # Other non-feature options
        features = "split-view";
        hyperlinks = true;
        line-numbers = true;
        navigate = true;
      };
    };
  };
}
