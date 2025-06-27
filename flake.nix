# Basis of inspiration:
#   https://www.chrisportela.com/posts/home-manager-flake
#   https://github.com/sebastiant/dotfiles/blob/master/flake.nix
{
  description = "SEH Home Manager Flake";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{
      self,
      flake-parts,
      home-manager,
      treefmt-nix,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } ({
      flake =
        let
          lib = import ./nix/lib { inherit inputs; };
        in
        {
          inherit lib;

          overlays = import ./nix/overlays { inherit inputs; };
          homeModules.default = import ./nix/home { inherit inputs; };
          homeConfigurations = import ./nix/home/machines { inherit inputs; };
          darwinModules.default = import ./nix/nix-darwin { inherit inputs; };
          darwinConfigurations = import ./nix/nix-darwin/machines { inherit inputs; };
        };

      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      perSystem =
        { pkgs, system, ... }:
        let
          # See https://github.com/numtide/treefmt-nix?tab=readme-ov-file#flakes.
          treefmtConfigured = treefmt-nix.lib.evalModule pkgs {
            projectRootFile = "flake.nix";
            programs = {
              buildifier.enable = true;
              gofumpt.enable = true;
              prettier.enable = true;
              shellcheck.enable = true;
              terraform.enable = true;
            };
            settings = {
              global = {
                excludes = [
                  "pnpm-lock.yaml"
                ];
              };
              formatter = {
                buildifier = {
                  includes = [
                    "*/*.bzl"
                    "*/BUILD.bazel"
                    "BUILD.bazel"
                    "MODULE.bazel"
                    "REPO.bazel"
                    "WORKSPACE.bazel"
                    "WORKSPACE.bzlmod"
                  ];
                };
                gofumpt = {
                  excludes = [
                    "*.gen.go"
                    "*/generated/*.go"
                  ];
                };
                shellcheck = {
                  includes = [
                    "*.bash"
                    "*.envrc"
                    "*.envrc.*"
                    "*.sh"
                  ];
                  options = [
                    "--external-sources"
                    "--source-path=SCRIPTDIR"
                  ];
                };
              };
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
          inherit checks formatter packages;

          apps = import ./nix/apps {
            inherit
              inputs
              pkgs
              system
              packages
              ;
          };
          #devShells = import ./nix/devshells { inherit pkgs formatter packages; };
        };
    });
}
