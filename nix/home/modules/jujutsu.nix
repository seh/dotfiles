{
  lib,
  pkgs,
  config,
  ...
}:

let
  cfg = config.dotfiles.jujutsu;
in
{
  options.dotfiles.jujutsu = {
    enable = lib.mkEnableOption "jujutsu";
    user = {
      name = lib.mkOption {
        type = lib.types.str;
        description = "Full common name for use in attribution";
      };
      email = lib.mkOption {
        type = lib.types.str;
        description = "Email address for use in commit attribution";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    programs.jujutsu = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.jujutsu;
      settings = {
        # See https://github.com/martinvonz/jj/blob/main/docs/config.md#configuration.
        user =
          lib.optionalAttrs (builtins.hasAttr "name" cfg.user) { name = cfg.user.name; }
          // lib.optionalAttrs (builtins.hasAttr "email" cfg.user) { email = cfg.user.email; };
      };
    };
  };
}
