# Reify "flake.flakeModules" so consumers can opt into specific
# subsets of this flake's flake-parts modules rather than importing
# everything wholesale.
#
# "flakeModules.default" is the minimal consumer-facing module: the
# schema for user-facing options ("./config.nix",
# "./nixpkgs-config.nix"), the "flake.lib" helpers
# ("./lib/default.nix", "./lib/flake-module.nix"), the "overlays"
# output ("./overlays/default.nix"), and
# "perSystem.dotfiles.callPackages" ("./packages/flake-modules.nix").
#
# The class aggregators under "./home", "./nix-darwin", and "./nixos"
# are deliberately excluded: publishing them into a consumer's
# evaluator would cause "nix flake check" there to warn about
# "homeModules", "darwinModules", and "nixosModules" as unknown
# flake outputs. "lib.mkHome", "lib.mkDarwin", and "lib.mkNixOS"
# reach the aggregators through the "dotfilesFlake" module argument
# set by "dotfilesFlakeArg" below, which closes over this flake's
# own "self" at evaluation time.
#
# The modules listed here self-activate in this flake via the
# top-level "import-tree" in "./flake.nix"; their appearance here
# is solely for external consumers.
{inputs, ...}: let
  # Inject "dotfilesFlake" as a module argument carrying this
  # flake's own "self", captured at this file's evaluation time.
  dotfilesFlakeArg = {
    _module.args.dotfilesFlake = inputs.self;
  };
in {
  imports = [
    inputs.flake-parts.flakeModules.flakeModules
    dotfilesFlakeArg
  ];

  flake.flakeModules = {
    default = {
      imports = [
        dotfilesFlakeArg
        ./config.nix
        ./nixpkgs-config.nix
        ./lib/default.nix
        ./lib/flake-module.nix
        ./overlays/default.nix
        ./packages/flake-modules.nix
      ];
    };
    checks = ./checks.nix;
    style = ./style.nix;
  };
}
