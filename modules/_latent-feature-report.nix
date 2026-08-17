# Publishes the latent-feature report as a consumer flake's
# "latentFeatures" output. A consumer imports it as the
# "inputs.dotfiles.modules.flake.latentFeatures" module and reads it
# with:
#
#   nix eval --raw .#latentFeatures.report
#   nix eval --raw .#latentFeatures.darwinConfigurations.<host>.report
#   nix eval --json .#latentFeatures.homeConfigurations.<name>.resolvedFor
#
# Grafting a rendering onto each configuration's own entry, rather
# than rendering everything at once, is what keeps a query cheap:
# asking after one configuration forces that one alone, and no managed
# user's nested evaluator runs until a query forces it.
#
# The module closes over this flake's own library, since a consumer's
# flake-parts evaluator has no "flakeLib" module argument.
#
# The leading underscore in the filename excludes this file from the
# "import-tree" call in the "../flake.nix" file; the
# "modules/flake-modules.nix" file imports it.
{flakeLib}: {
  config,
  lib,
  ...
}: let
  collected = flakeLib.collectLatentFeatures {
    darwinConfigurations = config.flake.darwinConfigurations or {};
    homeConfigurations = config.flake.homeConfigurations or {};
    nixosConfigurations = config.flake.nixosConfigurations or {};
  };
in {
  flake.latentFeatures =
    lib.mapAttrs (
      _output:
        lib.mapAttrs (
          _name: entry: entry // {report = flakeLib.renderConfiguration entry;}
        )
    )
    collected
    // {
      report = flakeLib.renderLatentFeatures collected;
    };
}
