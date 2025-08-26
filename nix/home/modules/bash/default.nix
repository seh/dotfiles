{
  config,
  lib,
  ...
}:

let
  cfg = config.dotfiles.bash;
in
{
  options.dotfiles.bash = {
    enable = lib.mkEnableOption "bash";
  };

  config = lib.mkIf cfg.enable {
    # TODO(seh): Elaborate this configuration.
    programs.bash = {
      enable = true;
    };
  };
}
