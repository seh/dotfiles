# Basis of inspiration:
#  https://github.com/sebastiant/dotfiles/blob/master/hosts/macbook/darwin-configuration.nix
{ config, lib, nixpkgs, pkgs, ... }:
{
  imports = [
    ../../programs/allow-non-free.nix
  ];

  environment.systemPackages = with pkgs;
    [
      docker
      home-manager
    ];

  users = {
    users.seh = {
      home = /Users/seh;
    };
  };

  # TODO(seh): Do we need this?
  nix = {
    nixPath = lib.mkForce [
      "nixpkgs=${nixpkgs}"
    ];
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
    
  services.nix-daemon.enable = true;  
  system.stateVersion = 4;
}
