# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/1c190d0ac1d87c159b8b7d777f02261ae58a3fc5/nix/home/profiles/minimal.nix#L12
{
  config,
  lib,
  ...
}:

let
  inherit (lib)
    mkIf
    mkOption
    types
    ;
in
{
  options.dotfiles.profiles.minimal.enable = mkOption {
    type = types.bool;
    default = true;
    description = "Whether to enable the bare minimum packages to make the dotfiles useful.";
  };

  config = mkIf config.dotfiles.profiles.minimal.enable {
    home.packages = # with pkgs;
      [
        # TODO(seh): Do we need to specify any here?
      ];

    programs.direnv = {
      enable = true;
      nix-direnv = {
        enable = true;
      };
    };

    programs.fzf = {
      enable = true;
      defaultOptions = [
        "--info=inline"
        "--bind=ctrl-r:toggle-sort"
      ];
    };
  };
}
