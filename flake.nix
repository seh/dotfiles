# Basis of inspiration:
#   https://www.chrisportela.com/posts/home-manager-flake
#   https://github.com/sebastiant/dotfiles/blob/master/flake.nix
{
  description = "SEH Home Manager Flake";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nix-darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, home-manager, treefmt-nix, ... } @ inputs:
    let lib = import ./nix/lib { inherit inputs; }; in
    {
      inherit lib;

      overlays = import ./nix/overlays { inherit inputs; };

      homeModules.default = import ./nix/home { inherit inputs; };
      homeConfigurations = import ./nix/home/machines { inherit inputs; };

      darwinModules.default = import ./nix/nix-darwin { inherit inputs; };
      darwinConfigurations = import ./nix/nix-darwin/machines { inherit inputs; };
    } // (lib.eachSupportedSystemPkgs ({ system, pkgs }:
      # TODO(seh): Activate more of this as the needs arise.
      let
        # See https://github.com/numtide/treefmt-nix?tab=readme-ov-file#flakes.
        treefmtConfigured = treefmt-nix.lib.evalModule
          pkgs
          {
            projectRootFile = "flake.nix";
            programs = {
              nixpkgs-fmt.enable = true;
              prettier.enable = true;
              shellcheck.enable = true;
            };
            settings.formatter.shellcheck = {
              options = [
                "--external-sources"
                "--source-path=SCRIPTDIR"
              ];
            };
          };
        # For "nix fmt":          
        formatter = treefmtConfigured.config.build.wrapper;
        # For "nix flake check":
        checks = {
          formatting = treefmtConfigured.config.build.check self;
        };
        packages = import ./nix/packages { inherit pkgs inputs; };
      in
      {
        inherit formatter packages;

        apps = import ./nix/apps { inherit inputs pkgs system packages; };
        #devShells = import ./nix/devshells { inherit pkgs formatter packages; };
      }));
}
