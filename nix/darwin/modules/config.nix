# nix-darwin configuration
#
# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/7cdd097dd01e0678b6ff56487689c78469237722/nix/darwin/modules/config.nix
{ config, lib, pkgs, dotfiles, ... }:

let
  nix = dotfiles.lib.config.nix.package;
in
{
  nix.package = lib.mkDefault pkgs.nixVersions.${nix};
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';
  #nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "1password"
    "1password-cli"
    #"discord"
    "dropbox"
    "slack"
    "zoom"
  ];
  # Create /etc/zshrc that loads the nix-darwin environment.
  # See https://github.com/LnL7/nix-darwin/issues/177.
  programs.zsh.enable = true;
  services.nix-daemon.enable = lib.mkDefault true;  

  environment.systemPackages = with pkgs;
    [
      docker
    ];

  # TODO(seh): Consider adapting more of the example configuration,
  # such as environment variables and system defaults.
}
