# Basis of inspiration:
#   https://www.chrisportela.com/posts/home-manager-flake
#   https://github.com/sebastiant/dotfiles/blob/master/flake.nix
{
  description = "SEH Home Manager Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, darwin, home-manager, ... }:
    let
      homeManagerConfFor = config:
        { ... }: {
          nixpkgs.overlays = [
            # TODO(seh): Define nixpkgs.overlays here.
          ];
          imports = [ config ];
        };
      darwinSystem = darwin.lib.darwinSystem {
        system = "x86_64-darwin";
        modules = [
          ./hosts/macbook/darwin-configuration.nix
          home-manager.darwinModules.home-manager
          {
            home-manager.users.seh =
              homeManagerConfFor ./hosts/macbook/home.nix;
          }
        ];
        specialArgs = { inherit nixpkgs; };
      };
    in {
      defaultPackage.x86_64-darwin = darwinSystem.system;
    };
}
