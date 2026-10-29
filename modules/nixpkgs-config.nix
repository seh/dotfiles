# Assemble the unfree-package toleration set that this flake's
# features declare, publish it, and apply it to the flake-parts
# evaluator's own package set. Each feature lists the unfree packages
# it installs through the "unfreePackages" argument of the "mkFeature"
# function, beside the packages themselves; the
# "dotfiles.unfreePackages" registry (see "./module-schema.nix")
# unions those contributions. Publishing the union as
# "flake.allowUnfreePackages" makes it available to the "pkgsFor"
# function and to the "mkDarwin" and "mkNixOS" functions (see
# "./lib/_constructors.nix"), which read this flake's outputs rather
# than its flake-parts configuration, so every package set starts from
# the same toleration set.
#
# This module assembles the union at the flake level because the
# flake-parts evaluator and the "pkgsFor" function instantiate nixpkgs
# outside any class evaluator, where nixpkgs' own
# "allowUnfreePackages" option is unavailable. Within a configuration,
# the constructors hand the union to that option, which merges
# additively, so a module there may add the unfree packages it
# installs.
{
  config,
  inputs,
  lib,
  ...
}: let
  inherit (config.dotfiles) unfreePackages;
in {
  flake.allowUnfreePackages = unfreePackages;

  perSystem = {system, ...}: {
    _module.args.pkgs = lib.mkForce (import inputs.nixpkgs {
      inherit system;
      config.allowUnfreePackages = unfreePackages;
    });
  };
}
