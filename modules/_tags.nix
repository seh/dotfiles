# Activation substrate shared by Home Manager, nix-darwin, and NixOS
# configurations.
#
# This module declares the options that describe hosts and the
# currently-active host's resolved record, plus the set of "known
# profile and feature names" that profile and feature modules
# advertise. It is imported by each of
# "flake.modules.homeManager.default", "flake.modules.darwin.default",
# and "flake.modules.nixos.default".
{
  lib,
  config,
  ...
}: let
  inherit (lib) mkOption types;

  flakeLib = config.dotfiles._flakeLib;
in {
  options.dotfiles = {
    hosts = mkOption {
      type = types.attrsOf (
        types.submodule {
          options = {
            framework = mkOption {
              type = types.enum [
                "homeManager"
                "nixDarwin"
                "nixOS"
              ];
              description = "Which configuration framework this host uses.";
            };
            platform = mkOption {
              type = types.str;
              description = "Nixpkgs system string, e.g. \"aarch64-darwin\".";
            };
            profiles = mkOption {
              type = types.listOf types.str;
              default = [];
              description = "Profile names this host opts into.";
            };
            features = mkOption {
              type = types.listOf types.str;
              default = [];
              description = "Feature names this host opts into.";
            };
          };
        }
      );
      default = {};
      description = ''
        Declarative registry of all hosts managed by this flake or a
        downstream instantiation flake. Each entry names a host and
        records its framework, platform, and profile/feature activations.
      '';
    };

    host = mkOption {
      type = types.submodule {
        options = {
          name = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = "Resolved host name, or null when unset.";
          };
          framework = mkOption {
            type = types.nullOr (
              types.enum [
                "homeManager"
                "nixDarwin"
                "nixOS"
              ]
            );
            default = null;
          };
          platform = mkOption {
            type = types.nullOr types.str;
            default = null;
          };
          profiles = mkOption {
            type = types.listOf types.str;
            default = [];
            description = ''
              Profile names this host opts into. These seed the
              typed cascade walk in "flake.lib.expandClosure",
              which produces "_host.activeProfiles" and
              "_host.activeFeatures".
            '';
          };
          features = mkOption {
            type = types.listOf types.str;
            default = [];
            description = ''
              Feature names this host opts into directly (outside
              of any profile that would pull them in). These seed
              the typed cascade walk together with "profiles".
            '';
          };
          excludeProfiles = mkOption {
            type = types.listOf types.str;
            default = [];
            description = ''
              Profile names to delete from this host's cascade
              before the closure walk. Both their out-edges (the
              dependencies they would advertise) and their
              in-edges (other profiles that target them) are
              removed, so anything reachable only through an
              excluded profile is automatically absent from the
              resolved closure.
            '';
          };
          excludeFeatures = mkOption {
            type = types.listOf types.str;
            default = [];
            description = ''
              Feature names to delete from this host's cascade
              before the closure walk. A feature still reachable
              through a non-excluded path remains active; one
              reachable only through excluded vertices drops out.
            '';
          };
        };
      };
      default = {};
      description = ''
        User-facing host record. Set by "lib.mkHome",
        "lib.mkDarwin", and "lib.mkNixOS" from the "host = {...}"
        argument; consumer modules may extend its "profiles",
        "features", "excludeProfiles", and "excludeFeatures" lists
        via the module system's append-merge.
      '';
    };

    _host = mkOption {
      type = types.submodule {
        options = {
          activeProfiles = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            description = ''
              Profiles in effect after deleting
              "host.excludeProfiles" from the cascade and walking
              the typed closure from "host.profiles".
            '';
          };
          activeFeatures = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            description = ''
              Features in effect after deleting
              "host.excludeProfiles" / "host.excludeFeatures" from
              the cascade and walking the typed closure from
              "host.profiles" and "host.features". A feature
              reachable only through an excluded profile is
              automatically absent.
            '';
          };
          activatesProfile = mkOption {
            type = types.functionTo types.bool;
            readOnly = true;
            description = ''
              Predicate testing whether a profile name is present
              in "activeProfiles".
            '';
          };
          activatesFeature = mkOption {
            type = types.functionTo types.bool;
            readOnly = true;
            description = ''
              Predicate testing whether a feature name is present
              in "activeFeatures".
            '';
          };
          unreachableProfiles = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            description = ''
              Profiles advertised via "dotfiles._knownProfiles"
              that this host's resolved closure ("activeProfiles")
              does not include. Exposed as a diagnostic aid.
            '';
          };
          unreachableFeatures = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            description = ''
              Features advertised via "dotfiles._knownFeatures"
              that this host's resolved closure ("activeFeatures")
              does not include. Exposed as a diagnostic aid.
            '';
          };
        };

        config = let
          inherit (config.dotfiles) host;
          hasCascadeLib =
            flakeLib != null && flakeLib ? expandClosure && flakeLib ? cascadesFor && flakeLib ? pruneCascades;
          cascades =
            if hasCascadeLib
            then
              flakeLib.cascadesFor {
                inherit (host) framework;
                isDarwin = host.framework == "nixDarwin";
                knownProfiles = config.dotfiles._knownProfiles;
              }
            else null;
          knownByRole = {
            profiles = config.dotfiles._knownProfiles;
            features = config.dotfiles._knownFeatures;
          };
          seed = {
            inherit (host) profiles features;
          };
          prunedCascades =
            if hasCascadeLib
            then
              flakeLib.pruneCascades cascades {
                profiles = host.excludeProfiles;
                features = host.excludeFeatures;
              }
            else null;
          prunedSeed = {
            profiles = lib.subtractLists host.excludeProfiles host.profiles;
            features = lib.subtractLists host.excludeFeatures host.features;
          };
          # Closure on the pruned graph: the activation answer.
          expanded =
            if hasCascadeLib
            then flakeLib.expandClosure prunedCascades knownByRole prunedSeed
            else seed;
          activeProfiles = expanded.profiles;
          activeFeatures = expanded.features;
        in {
          inherit activeProfiles activeFeatures;
          activatesProfile = name: builtins.elem name activeProfiles;
          activatesFeature = name: builtins.elem name activeFeatures;
          unreachableProfiles = lib.subtractLists activeProfiles config.dotfiles._knownProfiles;
          unreachableFeatures = lib.subtractLists activeFeatures config.dotfiles._knownFeatures;
        };
      };
      default = {};
      description = ''
        Computed activation record derived from "dotfiles.host"
        and the cascade table. All fields are read-only.
      '';
    };

    # "knownProfiles" and "knownFeatures" are intentionally flake-wide
    # registries, not class-scoped. A name advertised by any module in
    # any class (home-manager, darwin, or nixos) is accepted in any
    # host's declaration regardless of that host's class.
    #
    # This shape lets a concept like "development machine" be declared
    # once at the host level and hook-able by any class whose behavior
    # is appropriate, implemented differently as each class sees fit.
    # Today "development" has only a home-manager implementation in
    # "modules/home/profiles/development.nix", but the same name can
    # be declared on a NixOS or darwin host; activation picks up
    # whichever class-specific profile files exist for that name and
    # does nothing in classes where no such file does.
    #
    # Assertion consequence: typos are caught (a misspelled profile
    # or feature name fails the unknown-name check in
    # "modules/_assertions.nix"), but cross-class declarations are
    # accepted as intended.
    _knownProfiles = mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        Profile names that imported profile modules declare they
        respond to. Accumulated via "listOf"'s append-merge
        semantics. Used to catch typos in a host's declared
        "profiles" list and to diagnose role mismatches.
      '';
    };

    _knownFeatures = mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        Feature names that imported feature or profile modules
        declare they respond to. Accumulated via "listOf"'s
        append-merge semantics. Used to catch typos in a host's
        declared "features" list and to diagnose role mismatches.
      '';
    };

    _flakeLib = mkOption {
      type = with types; nullOr (lazyAttrsOf raw);
      default = null;
      internal = true;
      description = ''
        Proxy into "flake.lib" for use by this module's typed
        activation closure ("activeProfiles", "activeFeatures").
        Populated by each class aggregator. Kept nullable so that
        direct instantiations of this module (e.g. for tests) remain
        possible without a flake-parts context.
      '';
    };
  };

  # Diagnose exclusions that name a known item which is already
  # unreachable once the other exclusions have been applied: the
  # exclusion has no effect and may be removed. For each candidate
  # name "n" in "excludeProfiles" (or "excludeFeatures"), recompute
  # the closure with "n" temporarily removed from its exclusion
  # list (but with all other exclusions still pruning the graph).
  # If "n" is not in the resulting closure, it would not have been
  # active anyway, so listing it as excluded changes nothing.
  config = let
    inherit (config.dotfiles) host;
    flakeLib = config.dotfiles._flakeLib;
    hasCascadeLib =
      flakeLib != null && flakeLib ? expandClosure && flakeLib ? cascadesFor && flakeLib ? pruneCascades;
    hostLabel = toString host.name;
    knownByRole = {
      profiles = config.dotfiles._knownProfiles;
      features = config.dotfiles._knownFeatures;
    };
    cascades =
      if hasCascadeLib
      then
        flakeLib.cascadesFor {
          inherit (host) framework;
          isDarwin = host.framework == "nixDarwin";
          knownProfiles = config.dotfiles._knownProfiles;
        }
      else null;
    seed = {
      inherit (host) profiles features;
    };
    # The unpruned closure also forces the schema-level checks
    # inside "expandClosure" (dangling edges, feature edges that
    # target profiles) to run against the full table. Pruning
    # could otherwise hide a typo in an excluded profile's
    # adjacency list.
    _unprunedSideEffect =
      if hasCascadeLib
      then flakeLib.expandClosure cascades knownByRole seed
      else null;
    # Redundancy test: a name "n" excluded under "role" is
    # redundant when, with "n" removed from its own exclusion
    # list (but every other exclusion still pruning), the
    # resulting closure does not contain "n" anyway.
    isRedundant = role: name: let
      excluded = {
        profiles = host.excludeProfiles;
        features = host.excludeFeatures;
      };
      withoutSelf =
        excluded
        // {
          ${role} = lib.filter (m: m != name) excluded.${role};
        };
      pruned = flakeLib.pruneCascades cascades withoutSelf;
      seedHere = {
        profiles = lib.subtractLists withoutSelf.profiles host.profiles;
        features = lib.subtractLists withoutSelf.features host.features;
      };
      closure = flakeLib.expandClosure pruned knownByRole seedHere;
    in
      !(builtins.elem name (closure.${role} or []));
    redundantOf = role: known: excluded:
      if hasCascadeLib
      then builtins.filter (n: builtins.elem n known && isRedundant role n) excluded
      else [];
    redundantProfiles = redundantOf "profiles" config.dotfiles._knownProfiles host.excludeProfiles;
    redundantFeatures = redundantOf "features" config.dotfiles._knownFeatures host.excludeFeatures;
    mkWarning = role: option: name: ''
      Resolving host "${hostLabel}": ${option} entry "${name}" names a known ${role} that is already unreachable in the cascade closure; the exclusion has no effect and may be removed.
    '';
  in {
    warnings = lib.seq _unprunedSideEffect (
      map (mkWarning "profile" "excludeProfiles") redundantProfiles
      ++ map (mkWarning "feature" "excludeFeatures") redundantFeatures
    );
  };
}
