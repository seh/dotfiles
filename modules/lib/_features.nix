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
# Each value is one of two shapes:
#
#   1. A deferred module returning the *contents* of a "config"
#      block — not a full module with its own "config = {...}" key.
#      The helper inserts the
#      "config = lib.mkIf (activatesFeature name) (...)" wrapper, so
#      a body that itself wraps in "config = {...}" would produce
#      "config.config = {...}" and silently drop its contributions.
#
#   2. A structured attrset "{options? = ...; config? = ...;}" for
#      features that declare their own options. The "options" half
#      is a normal module function (e.g. "{lib, ...}: {options = ...;}")
#      passed through verbatim — option declarations cannot be
#      conditional. The "config" half follows the same contract as
#      shape 1 and gets the same activation gate. Either key may be
#      omitted.
#
# Conditional sub-contributions inside a config body should use
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

  # Build the per-class deferred module for one body, dispatching
  # on its shape. A function body is the plain shape (config-only,
  # gated). An attrset with at least one of "options"/"config" is
  # the structured shape (options pass through, config gated). The
  # result is "lib.mkMerge" of the parts so the module evaluator
  # sees one contribution per class.
  buildClassModule = predicate: name: body:
    if lib.isFunction body
    then wrap predicate name body
    else if lib.isAttrs body && (body ? options || body ? config)
    then
      lib.mkMerge (
        lib.optional (body ? options) body.options
        ++ lib.optional (body ? config) (wrap predicate name body.config)
      )
    else throw "mkFeature/mkProfile: body for \"${name}\" must be a function or an attrset with \"options\" and/or \"config\"";

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
            ${name} = buildClassModule predicate name body;
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
