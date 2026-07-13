# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/1c190d0ac1d87c159b8b7d777f02261ae58a3fc5/nix/home/profiles/development.nix
{flakeLib, ...}:
flakeLib.mkProfile "development" {
  homeManager = {lib, ...}: {
    dotfiles = {
      coder = {
        enableSSHIntegration = lib.mkDefault true;
      };
    };
  };
}
