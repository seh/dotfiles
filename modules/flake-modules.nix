# Reify a handful of this flake's flake-parts modules under
# "flake.modules.flake" so consumers can opt into them from their own
# evaluators.
#
# The "flake.modules.flake.style" module is a consumer-facing
# extension: a consumer importing it installs the corresponding
# "perSystem" contributions into its own evaluator.
#
# "flake.modules.flake.latentFeatures" is another: a consumer
# importing it gains the "latentFeatures" output, the on-demand report
# described in the "_latent-feature-report.nix" file. That module
# closes over this flake's own library, which the "flakeLib" module
# argument carries here and no consumer evaluator has.
#
# There is deliberately no "flake.modules.flake.default": the
# constructors ("lib.mkHome", "lib.mkDarwin", "lib.mkNixOS") are
# called as plain library functions through "inputs.dotfiles.lib.*",
# closing over this flake's own "self" without installing anything
# into the consumer evaluator. All option schemas and overlays
# relevant to a consumer flow in through the modules passed to those
# constructors, inside the target class evaluator where they are
# actually read.
#
# Importing "inputs.flake-parts.flakeModules.modules" pulls in the
# class-aware "flake.modules.<class>.<name>" option schema, which
# enforces module-class checking and supersedes the legacy flat
# "flake.{home,darwin,nixos,flake}Modules" passthroughs.
{
  flakeLib,
  inputs,
  ...
}: {
  imports = [
    inputs.flake-parts.flakeModules.modules
  ];
  flake.modules.flake = {
    latentFeatures = import ./_latent-feature-report.nix {inherit flakeLib;};
    style = ./style.nix;
  };
}
