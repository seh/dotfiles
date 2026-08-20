# Activation substrate shared by Home Manager, nix-darwin, and NixOS
# configurations.
#
# This module declares the options that describe the host and the
# currently-active host's resolved record, plus the set of "known
# profile and feature names" that profile and feature modules
# advertise. It is imported by each of
# "flake.modules.homeManager.default", "flake.modules.darwin.default",
# and "flake.modules.nixos.default". It imports the "_users.nix"
# declaration module so that the "dotfiles.users" registry is
# visible wherever this module evaluates: on a system host the
# active set unions the machine's own activation with each user's,
# so the registry must be readable even in the home-manager class,
# where it stays empty.
{
  lib,
  config,
  # Nullable so that direct instantiations of this module (e.g. for
  # tests) remain possible outside an evaluator that provides a
  # package set. When present, it supplies the detected platform
  # exposed as the "dotfiles._host.platform" option below.
  pkgs ? null,
  ...
}: let
  inherit (lib) mkOption types;
  inherit (config.dotfiles) host;
  inherit (import ./lib/_option-types.nix {inherit lib;}) setOfNames;

  # Detect the host's platform from the evaluating package set. Every
  # real evaluator (Home Manager, nix-darwin, NixOS) provides one, so
  # the detected value is authoritative; host records carry no
  # platform attribute. Null arises only in a bare instantiation of
  # this module without a package set.
  platform =
    if pkgs != null
    then pkgs.stdenv.hostPlatform.system
    else null;

  flakeLib = config.dotfiles._flakeLib;
  hasImplicationsLib =
    flakeLib != null && flakeLib ? implicationsFor && flakeLib ? resolveActivation;
  knownByRole = {
    profiles = config.dotfiles._knownProfiles;
    features = config.dotfiles._featureUniverse;
  };
  preconditions = config.dotfiles._featurePreconditions;
  implications =
    if hasImplicationsLib
    then
      flakeLib.implicationsFor {
        inherit platform;
        knownProfiles = config.dotfiles._knownProfiles;
        profileSupportedPlatforms = config.dotfiles._profileSupportedPlatforms;
        impliedEdges = config.dotfiles._impliedEdges;
      }
    else null;
  # The machine's own selected profiles and features. See the
  # "dotfiles.host" submodule description for the system-host and
  # standalone-host readings.
  selected = {
    inherit (host) profiles features;
  };
  # Fold the machine-wide forbid lists into a selector's own
  # exclusions: "host.forbidProfiles"/"host.forbidFeatures" prune
  # every selector's walk before "resolveActivation", so a forbidden
  # name activates nowhere. Because the host record conveys the two
  # lists and keeps them intact when that record is mirrored into each
  # nested home-manager evaluator (see the propagation module in
  # "modules/lib/_constructors.nix"), each user's own home environment
  # respects them through this same logic, with no extra wiring.
  withForbidden = excluded: {
    profiles = excluded.profiles ++ host.forbidProfiles;
    features = excluded.features ++ host.forbidFeatures;
  };
  # Every selector's activation, resolved coherently and
  # independently: the machine's own walk plus one walk per user in
  # the "dotfiles.users" registry. Each walk runs the full
  # preconditions table on that selector's own selections, so a
  # contingent feature activates within a selector exactly when that
  # selector's own set satisfies its preconditions, never across
  # selectors. The active set below is the union of these results.
  machineActivation =
    if hasImplicationsLib
    then
      flakeLib.resolveActivation {
        inherit implications knownByRole selected preconditions;
        excluded = withForbidden {
          profiles = host.excludeProfiles;
          features = host.excludeFeatures;
        };
      }
    else selected;
  perUserActivation =
    if hasImplicationsLib
    then
      lib.mapAttrs (
        _: userCfg:
          flakeLib.resolveActivation {
            inherit implications knownByRole preconditions;
            selected = {
              inherit (userCfg) profiles features;
            };
            excluded = withForbidden {
              profiles = userCfg.excludeProfiles;
              features = userCfg.excludeFeatures;
            };
          }
      )
      config.dotfiles.users
    else {};
  # The active set: the union of every selector's coherent
  # activation.
  activeSet = {
    profiles = lib.unique (
      machineActivation.profiles
      ++ lib.concatMap (a: a.profiles) (lib.attrValues perUserActivation)
    );
    features = lib.unique (
      machineActivation.features
      ++ lib.concatMap (a: a.features) (lib.attrValues perUserActivation)
    );
  };
in {
  imports = [./_users.nix];

  options.dotfiles = {
    host = mkOption {
      type = types.submodule {
        options = {
          name = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = "Resolved host name, or null when unset.";
          };
          profiles = mkOption {
            type = types.listOf types.str;
            default = [];
            description = ''
              Profile names the machine itself selects. Expansion
              starts from these selected names.
            '';
          };
          features = mkOption {
            type = types.listOf types.str;
            default = [];
            description = ''
              Feature names the machine itself selects directly
              (outside of any profile that would pull them in).
              Expansion starts from these selected names together
              with "profiles".
            '';
          };
          excludeProfiles = mkOption {
            type = types.listOf types.str;
            default = [];
            description = ''
              Profile names to delete from this host's implication
              graph before the activation walk. Both their out-edges
              (the dependencies they would advertise) and their in-edges
              (other profiles that target them) are removed, so
              anything reachable only through an excluded profile is
              automatically absent from the resolved activation.
            '';
          };
          excludeFeatures = mkOption {
            type = types.listOf types.str;
            default = [];
            description = ''
              Feature names to delete from this host's implication
              graph before the activation walk. A feature still
              reachable through a non-excluded path remains active; one
              reachable only through excluded vertices drops out.
              Excluding a contingent feature by name keeps it inactive
              even when its preconditions are all met.
            '';
          };
          forbidProfiles = mkOption {
            type = setOfNames {merge = "union";};
            default = [];
            description = ''
              Machine-wide forbidding: each named profile is pruned
              from every selector's activation, the machine's own and
              every user's, so it activates nowhere and no user
              receives it. Forbidding is the machine-wide counterpart
              to the per-selector "excludeProfiles", which prunes only
              its own selector's walk.
            '';
          };
          forbidFeatures = mkOption {
            type = setOfNames {merge = "union";};
            default = [];
            description = ''
              Machine-wide forbidding: each named feature is pruned
              from every selector's activation, the machine's own and
              every user's, so it activates nowhere and no user
              receives it. Forbidding is the machine-wide counterpart
              to the per-selector "excludeFeatures", which prunes only
              its own selector's walk.
            '';
          };
        };
      };
      default = {};
      description = ''
        The machine's own host record: its selected "profiles" and
        "features", the exclusions it applies to its own walk, and
        the machine-wide "forbidProfiles"/"forbidFeatures" lists. The
        machine is a first-class selector, resolved alongside the
        users in "dotfiles.users". On a system host these fields
        carry the machine's own wants, standing alongside its users,
        and no longer absorb any union of its users' selections; on a
        standalone home-manager host the machine is the sole user, so
        they are that user's selections. Set by "lib.mkHome",
        "lib.mkDarwin", and "lib.mkNixOS" from the "host = {...}"
        argument; consumer modules may extend its lists via the
        module system's append-merge.
      '';
    };

    _host = mkOption {
      type = types.submodule {
        options = {
          platform = mkOption {
            type = types.nullOr types.str;
            readOnly = true;
            description = ''
              The host's platform: the Nixpkgs system identifier
              (e.g. "aarch64-darwin") detected from the evaluating
              package set, or null when this module is instantiated
              without one. Detection is the only source; host
              records carry no platform attribute.
            '';
          };
          activeProfiles = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            description = ''
              Profiles in effect on the machine: the union of every
              selector's coherent activation. The machine's own walk
              (from "host.profiles", with "host.excludeProfiles"
              deleted from the implication graph) unions with one walk
              per user in "dotfiles.users" (from that user's
              selections and exclusions).
            '';
          };
          activeFeatures = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            description = ''
              Features active on the machine: the union of every
              selector's coherent activation. Each selector's walk
              deletes its own exclusions from the implication graph,
              follows the remaining edges from its selected profiles
              and features, and activates every contingent feature
              whose preconditions that selector's own result meets. A
              contingent feature therefore activates within a single
              selector, never across selectors; a feature reachable
              only through an excluded profile is automatically
              absent. These active features decide whether each
              feature's system-class configuration applies, tested via
              "inEffect". An inactive feature contributes nothing, as
              though it were never defined. The walk reaches interests
              as well as features, since a precondition may cite
              either kind. This list holds the features alone; see
              "expressedInterests" for the interests.
            '';
          };
          expressedInterests = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            description = ''
              Interests expressed on the machine, whether a selector
              expressed one directly or another name implied it. An
              interest carries no configuration of its own; expressing
              one only completes the preconditions of the contingent
              features citing it. Declaring that an interest exists,
              via the "mkInterest" function, does not express it.
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
          inEffect = mkOption {
            type = types.functionTo types.bool;
            readOnly = true;
            description = ''
              Predicate answering whether a name is in effect in this
              host's scope: true for an active feature and for an
              expressed interest alike. Feature bodies consult this to
              decide whether their configuration applies.
            '';
          };
          inactiveProfiles = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            description = ''
              Profiles advertised via "dotfiles._knownProfiles" that
              are not active on the machine: absent from the active
              (union) set "activeProfiles", whatever keeps them out,
              whether no selector selected them, an exclusion pruned
              them, or they are unsupported on the machine's platform.
              Exposed as a diagnostic aid.
            '';
          };
          inactiveFeatures = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            description = ''
              Features advertised via "dotfiles._knownFeatures" that
              are not active on the machine, whatever keeps them out.
              Exposed as a diagnostic aid: each entry is a name a
              selector could select, so contingent features, which no
              selector may select, are left out; see "latentFeatures"
              for those.
            '';
          };
          unexpressedInterests = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            description = ''
              Interests advertised via "dotfiles._knownInterests" that
              no selector expresses. Exposed as a diagnostic aid: each
              entry is an interest a selector could express, leaving
              the contingent features citing it latent.
            '';
          };
          latentFeatures = mkOption {
            type = types.attrsOf (
              types.submodule {
                options = {
                  preconditions = mkOption {
                    type = types.listOf types.str;
                    description = ''
                      The feature's full preconditions list.
                    '';
                  };
                  missing = mkOption {
                    type = types.listOf types.str;
                    description = ''
                      The preconditions not present in the machine's
                      own coherent activation.
                    '';
                  };
                  excluded = mkOption {
                    type = types.bool;
                    description = ''
                      True when the machine names this feature in its
                      own "host.excludeFeatures" list.
                    '';
                  };
                };
              }
            );
            readOnly = true;
            description = ''
              Contingent features latent for the current evaluator's
              own selections, keyed by name. This diagnostic reflects
              one selector: the machine's own coherent activation,
              never the active (union) set. A contingent feature
              already active anywhere on the machine (present in
              "activeFeatures", whether the machine or a user
              activated it) is left out and never also reported
              latent; the entry for each of the rest carries the
              per-field reasons declared below. Each nested per-user
              evaluator computes its own "latentFeatures" from that
              user's selections.
            '';
          };
        };

        config = let
          # The active set unions every selector's coherent activation
          # (see "machineActivation" / "perUserActivation" above).
          # Diagnostics that describe one selector read the machine's
          # own activation directly.
          activeProfiles = activeSet.profiles;
          # Every name the union reached: active features and
          # expressed interests together, since a precondition may
          # cite either kind. The two are published apart, so keep the
          # union to this scope.
          namesInEffect = activeSet.features;
          machineFeatures = machineActivation.features;
          isInterest = name: builtins.elem name config.dotfiles._knownInterests;
          contingentNames = builtins.attrNames config.dotfiles._featurePreconditions;
        in {
          inherit activeProfiles platform;
          activeFeatures = builtins.filter (name: !(isInterest name)) namesInEffect;
          expressedInterests = builtins.filter isInterest namesInEffect;
          activatesProfile = name: builtins.elem name activeProfiles;
          inEffect = name: builtins.elem name namesInEffect;
          inactiveProfiles = lib.subtractLists activeProfiles config.dotfiles._knownProfiles;
          inactiveFeatures =
            lib.subtractLists (namesInEffect ++ contingentNames) config.dotfiles._knownFeatures;
          unexpressedInterests =
            lib.subtractLists namesInEffect config.dotfiles._knownInterests;
          latentFeatures =
            lib.mapAttrs (name: preconditions: {
              inherit preconditions;
              missing = builtins.filter (r: !(builtins.elem r machineFeatures)) preconditions;
              excluded = builtins.elem name host.excludeFeatures;
            })
            (lib.filterAttrs (name: _: !(builtins.elem name machineFeatures))
              config.dotfiles._featurePreconditions);
        };
      };
      default = {};
      description = ''
        Computed activation record built from "dotfiles.host", the
        implication graph, and the detected platform. All fields are
        read-only.
      '';
    };

    # "knownProfiles" and "knownFeatures" are intentionally flake-wide
    # registries, not class-scoped. A name advertised by any module in
    # any class (home-manager, darwin, or nixos) is accepted in any
    # host's selections regardless of that host's class.
    #
    # This arrangement lets a concept like "development machine" be
    # selected once at the host level and hook-able by any class whose
    # behavior is appropriate, implemented differently as each class
    # sees fit. Today "development" has only a home-manager
    # implementation in "modules/home/profiles/development.nix", but
    # the same name can be selected on a NixOS or darwin host;
    # activation picks up whichever class-specific profile files exist
    # for that name and does nothing in classes where no such file
    # does.
    #
    # Assertion consequence: typos are caught (a misspelled profile
    # or feature name fails the unknown-name check in
    # "modules/_assertions.nix"), but cross-class selections are
    # accepted as intended.
    _knownProfiles = mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        Profile names that imported profile modules declare they
        respond to. Accumulated via "listOf"'s append-merge
        semantics. Used to catch typos in a host's selected
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
        selected "features" list and to diagnose role mismatches.
      '';
    };

    _knownInterests = mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        Interest names that imported modules declare via the
        "mkInterest" function. Mirrored from the flake-level
        "dotfiles.knownInterests" registry by each class aggregator.
        Folded into the feature universe for activation, exclusion,
        and preconditions, and kept apart so that kind-aware checks
        (such as the namespace-disjointness assertion in
        "modules/_assertions.nix") can tell interests from features.
      '';
    };

    _featureUniverse = mkOption {
      type = types.listOf types.str;
      readOnly = true;
      internal = true;
      description = ''
        The names usable wherever a feature name is expected: the
        known feature names and the known interest names, combined and
        de-duplicated. Interests share the feature universe—a host
        selects or excludes an interest, and a precondition may name
        it, exactly as with a feature—while the separate registries
        let kind-aware checks tell the kinds apart. Computed from
        "dotfiles._knownFeatures" and "dotfiles._knownInterests".
      '';
    };

    _featurePreconditions = mkOption {
      type = types.attrsOf (types.listOf types.str);
      default = {};
      description = ''
        Per-feature preconditions, keyed by feature name; each value
        names the features or interests that must all be active for
        the keyed contingent feature to activate. Mirrored from the
        flake-level "dotfiles.featurePreconditions" registry by each
        class aggregator. Consulted by the activation fixpoint in
        "flake.lib.expandActivation" and by the assertions in
        "modules/_assertions.nix".
      '';
    };

    _impliedEdges = mkOption {
      type = types.attrsOf (types.listOf types.raw);
      default = {};
      description = ''
        Per-source implied edges, keyed by source profile, feature,
        or interest name; each value is that source's "implies"
        list. Mirrored from the flake-level "dotfiles.impliedEdges"
        registry by each class aggregator. Passed to
        "flake.lib.implicationsFor", which assembles the role-keyed
        implication graph from these co-located declarations.
      '';
    };

    _profileSupportedPlatforms = mkOption {
      type = types.attrsOf (types.listOf types.str);
      default = {};
      description = ''
        Per-profile platform support, keyed by profile name; each
        value lists the platforms on which that profile may
        activate. Mirrored from the flake-level
        "dotfiles.profileSupportedPlatforms" registry by each class
        aggregator. Consulted by the implication-graph computation
        (the "all" entry skips unsupported profiles) and by the
        platform-support assertion in "modules/_assertions.nix".
      '';
    };

    _implications = mkOption {
      type = with types; nullOr raw;
      readOnly = true;
      internal = true;
      description = ''
        The implication graph computed once per evaluator by
        "flake.lib.implicationsFor" from the known profiles, the
        host's platform, and the per-profile platform support, or null
        when the implications library is unavailable. Read by the
        activation computation here and by the implied-target
        assertion in "modules/_assertions.nix", so the same evaluator
        builds the graph once.
      '';
    };

    _flakeLib = mkOption {
      type = with types; nullOr (lazyAttrsOf raw);
      default = null;
      internal = true;
      description = ''
        Proxy into "flake.lib" for use by this module's activation
        computation ("activeProfiles", "activeFeatures"). Populated by
        each class aggregator. Kept nullable so that direct
        instantiations of this module (e.g. for tests) remain
        possible without a flake-parts context.
      '';
    };
  };

  # Diagnose exclusions that name a known item the machine's own
  # selections do not activate: the exclusion has no effect on the
  # machine and may be removed. For each candidate name "n" in
  # "excludeProfiles" (or "excludeFeatures"), recompute the machine's
  # own activation with "n" temporarily removed from its exclusion
  # list (but with all other exclusions still pruning the graph). If
  # "n" is absent from the resulting activation, it would not have
  # been active anyway, so listing it as excluded changes nothing.
  # The judgment is against the machine's own selections, not the
  # active union: a name a user activates can still be inactive for
  # the machine, so the machine excluding it is genuinely redundant.
  config = let
    hostLabel = toString host.name;
    # The unpruned walk also forces the schema-level checks inside
    # "expandClosure" (dangling edges, feature edges that target
    # profiles) and the precondition-cycle check inside
    # "expandActivation" to run against the full tables. Pruning
    # could otherwise hide a typo in an excluded profile's
    # adjacency list, or a cycle behind an excluded member.
    _unprunedSideEffect =
      if hasImplicationsLib
      then
        flakeLib.resolveActivation {
          inherit implications knownByRole selected preconditions;
        }
      else null;
    # Redundancy test: a name "n" excluded under "role" is
    # redundant when, with "n" removed from its own exclusion
    # list (but every other exclusion still pruning), the
    # resulting activation does not contain "n" anyway. The test
    # runs the full fixpoint: an exclusion suppressing a contingent
    # feature that would otherwise activate has real effect, and a
    # walk that never activates contingent features would misreport it
    # as removable.
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
      activation = flakeLib.resolveActivation {
        inherit implications knownByRole selected preconditions;
        excluded = withoutSelf;
      };
    in
      !(builtins.elem name (activation.${role} or []));
    redundantOf = role: known: excluded:
      if hasImplicationsLib
      then builtins.filter (n: builtins.elem n known && isRedundant role n) excluded
      else [];
    redundantProfiles = redundantOf "profiles" config.dotfiles._knownProfiles host.excludeProfiles;
    redundantFeatures = redundantOf "features" config.dotfiles._featureUniverse host.excludeFeatures;
    mkWarning = role: option: name: let
      forbidOption =
        if role == "feature"
        then "forbidFeatures"
        else "forbidProfiles";
    in ''
      Resolving host "${hostLabel}": ${option} entry "${name}" names a known ${role} that this machine's own selections do not activate; the exclusion has no effect on the machine and may be removed. A machine-wide forbid list that keeps a ${role} inactive for every selector is spelled "dotfiles.host.${forbidOption}".
    '';
  in {
    dotfiles._featureUniverse = lib.unique (
      config.dotfiles._knownFeatures ++ config.dotfiles._knownInterests
    );

    dotfiles._implications = implications;

    warnings = lib.seq _unprunedSideEffect (
      map (mkWarning "profile" "excludeProfiles") redundantProfiles
      ++ map (mkWarning "feature" "excludeFeatures") redundantFeatures
    );
  };
}
