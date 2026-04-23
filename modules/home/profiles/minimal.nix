# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/1c190d0ac1d87c159b8b7d777f02261ae58a3fc5/nix/home/profiles/minimal.nix#L12
{
  flake.knownProfiles = ["minimal"];

  flake.profileModules.homeManager.minimal = {
    config,
    lib,
    ...
  }: {
    config = lib.mkIf (config.dotfiles._host.activatesProfile "minimal") {
      home.packages = [
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
  };
}
