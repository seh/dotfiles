# Helpers that build feature and profile modules with their
# activation gating wired in.
#
# A "feature" or "profile" file using these helpers takes the shape:
#
#   {flakeLib, ...}:
#     flakeLib.mkFeature "<name>" {
#       homeManager = {pkgs, ...}: {
#         home.packages = [pkgs.foo];
#       };
#       nixDarwin = ...;
#       nixOS = ...;
#     }
#
# The "bodies" attrset is keyed by class name as recognized by this
# flake's class aggregators ("homeManager", "nixDarwin", "nixOS").
# Each value is a deferred module that, by convention, returns the
# *contents* of a "config" block — not a full module with its own
# "config = {...}" key. The helper inserts the
# "config = lib.mkIf (activatesFeature name) (...)" wrapper, so a
# body that itself wraps in "config = {...}" would produce
# "config.config = {...}" and silently drop its contributions.
#
# Conditional sub-contributions inside a body should use
# "lib.mkMerge" / "lib.mkIf" at the value level, not at the module
# level.
#
# An empty bodies attrset ("{}") is allowed and produces a
# name-only registration (under "knownFeatures" or "knownProfiles")
# with no module contributions. Useful when a name is meaningful
# only as a cross-feature reference.
{lib}: let
  # Build a deferred module that gates "body" on
  # "host.<predicate> name", where "host" is the resolved
  # "config.dotfiles._host" and "predicate" is one of
  # "activatesFeature" / "activatesProfile".
  #
  # The wrapper's outer function destructures every module argument
  # that bodies might rely on, so the module system's argument
  # injection mechanism (driven by "builtins.functionArgs") can fill
  # them in from "_module.args". A body that destructures an
  # argument missing from this list will fail with "called without
  # required argument".
  #
  # Currently covers what the home-manager, nix-darwin, and NixOS
  # class evaluators provide for the features in this repository.
  # If a future feature body needs "inputs", "specialArgs", or any
  # other module argument, add it here as "name ? null".
  wrap = predicate: name: body: {
    config,
    lib,
    # deadnix: skip
    pkgs ? null,
    # deadnix: skip
    osConfig ? null,
    ...
  } @ args: {
    config = lib.mkIf (predicate config.dotfiles._host name) (body args);
  };

  # Common builder for both helpers. "knownKey" is the
  # accumulator-list option ("knownFeatures" / "knownProfiles");
  # "modulesKey" is the per-class deferred-module option
  # ("featureModules" / "profileModules"); "predicate" is the
  # activation predicate to apply at evaluation time.
  mk = {
    knownKey,
    modulesKey,
    predicate,
  }: name: bodies: {
    dotfiles =
      {
        ${knownKey} = [name];
      }
      // lib.optionalAttrs (bodies != {}) {
        ${modulesKey} =
          lib.mapAttrs (_class: body: {
            ${name} = wrap predicate name body;
          })
          bodies;
      };
  };
in {
  mkFeature = mk {
    knownKey = "knownFeatures";
    modulesKey = "featureModules";
    predicate = host: name: host.activatesFeature name;
  };

  mkProfile = mk {
    knownKey = "knownProfiles";
    modulesKey = "profileModules";
    predicate = host: name: host.activatesProfile name;
  };
}
