# Assemble the unfree-package toleration set that this flake's
# features declare, publish it for the constructors, and apply it to
# the flake-parts evaluator's own package set.
#
# Each feature names the unfree packages it installs through the
# "unfreePackages" argument of the "mkFeature" function, beside the
# packages themselves; the "dotfiles.unfreePackages" registry (see
# "./module-schema.nix") unions those contributions. Publishing the
# union as "flake.allowUnfreePackages" makes it available to the
# standalone "pkgsFor" function used by
# "lib.mkHome"/"lib.mkDarwin"/"lib.mkNixOS" (see
# "./lib/_constructors.nix"), which reads this flake's outputs rather
# than its flake-parts configuration. Both instantiation sites then
# hand nixpkgs the same set.
#
# The union arrives here rather than from inside the class evaluator
# where a feature installs its packages because nixpkgs' own
# "allowUnfreePackages" option, which merges additively across modules
# for exactly that purpose, is unavailable: these constructors assign
# the "nixpkgs.pkgs" option, and NixOS rejects any "nixpkgs.config"
# assignment beside it.
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
