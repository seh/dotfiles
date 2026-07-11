# Activation substrate shared by Home Manager, nix-darwin, and NixOS
# configurations.
#
# This module declares the options that describe hosts and the
# currently-active host's resolved record, plus the set of "known
# profile and feature names" that profile and feature modules
# advertise. It is imported by each of
# "flake.modules.homeManager.default", "flake.modules.darwin.default",
# and "flake.modules.nixos.default". It imports the "_users.nix"
# declaration module so that the "dotfiles.users" registry—from
# which the "_computedHostSelections" record below is computed—
# exists wherever this module evaluates.
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
  # With precomputed selections this evaluator activates no
  # contingent features on its own; see the description of the
  # "_hostSelectionsComputed" option below.
  applicablePreconditions =
    if config.dotfiles._hostSelectionsComputed
    then {}
    else config.dotfiles._featurePreconditions;
  implications =
    if hasImplicationsLib
    then
      flakeLib.implicationsFor {
        inherit platform;
        knownProfiles = config.dotfiles._knownProfiles;
        profileSupportedPlatforms = config.dotfiles._profileSupportedPlatforms;
      }
    else null;
  selected = {
    inherit (host) profiles features;
  };
  # The per-user activation union recorded in the
  # "_computedHostSelections" option; see that option's declaration
  # below for what the record means and who reads it. The per-user
  # walks consult the full preconditions table—contingent features
  # activate per user—regardless of how "applicablePreconditions"
  # above gates this evaluator's own walk.
  computedHostSelections =
    if config.dotfiles.users == {} || !hasImplicationsLib
    then null
    else let
      activePerUser =
        lib.mapAttrs (
          _: userCfg:
            flakeLib.resolveActivation {
              inherit implications knownByRole;
              preconditions = config.dotfiles._featurePreconditions;
              selected = {
                inherit (userCfg) profiles features;
              };
              excluded = {
                profiles = userCfg.excludeProfiles;
                features = userCfg.excludeFeatures;
              };
            }
        )
        config.dotfiles.users;
    in {
      profiles = lib.unique (lib.concatMap (a: a.profiles) (lib.attrValues activePerUser));
      features = lib.unique (lib.concatMap (a: a.features) (lib.attrValues activePerUser));
    };
in {
  imports = [./_users.nix];

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
        records its framework, platform, and profile/feature selections.
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
          profiles = mkOption {
            type = types.listOf types.str;
            default = [];
            description = ''
              Profile names this host opts into. Expansion starts
              from these selected names and produces
              "_host.activeProfiles" and "_host.activeFeatures".
            '';
          };
          features = mkOption {
            type = types.listOf types.str;
            default = [];
            description = ''
              Feature names this host opts into directly (outside
              of any profile that would pull them in). Expansion
              starts from these selected names together with
              "profiles".
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
              Profiles in effect after deleting "host.excludeProfiles"
              from the implication graph and walking the remaining
              edges from "host.profiles".
            '';
          };
          activeFeatures = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            description = ''
              Features active in this host's scope: their
              configuration for this evaluator's class applies. The
              walk deletes "host.excludeProfiles" and
              "host.excludeFeatures" from the implication graph,
              follows the remaining edges from "host.profiles" and
              "host.features", and activates every contingent feature
              whose preconditions the result meets; a feature
              reachable only through an excluded profile is
              automatically absent. An inactive feature contributes
              nothing, as though it were never defined.

              The walk reaches interests as well as features, since a
              precondition may cite an interest. This list holds the
              features alone; see "expressedInterests" for the
              interests.
            '';
          };
          expressedInterests = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            description = ''
              Interests expressed in this host's scope, whether this
              host expressed one directly or another name implied it.
              An interest carries no configuration of its own;
              expressing one only completes the preconditions of the
              contingent features citing it. Declaring that an
              interest exists, via the "mkInterest" function, does not
              express it.
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
              Profiles advertised via "dotfiles._knownProfiles"
              that this host's resolved activation
              ("activeProfiles") does not include, whatever keeps
              them inactive: not selected, pruned by an exclusion,
              or unsupported on the host's platform. Exposed as a
              diagnostic aid.
            '';
          };
          inactiveFeatures = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            description = ''
              Features advertised via "dotfiles._knownFeatures" that
              this host's resolved activation leaves inactive,
              whatever keeps them out. Exposed as a diagnostic aid:
              each entry is a name this host could select, so
              contingent features, which no host may select, are left
              out; see "latentFeatures" for those.
            '';
          };
          unexpressedInterests = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            description = ''
              Interests advertised via "dotfiles._knownInterests" that
              this host's scope does not express. Exposed as a
              diagnostic aid: each entry is an interest this host
              could express, leaving the contingent features that name
              it latent.
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
                      The preconditions not currently active on this
                      host.
                    '';
                  };
                  excluded = mkOption {
                    type = types.bool;
                    description = ''
                      True when the host names this feature in its
                      "excludeFeatures" list.
                    '';
                  };
                };
              }
            );
            readOnly = true;
            description = ''
              Contingent features that have not activated on this
              host, keyed by name. Each entry says why its feature
              stays latent: the "preconditions" field holds the
              feature's full preconditions list, the "missing" field
              holds the preconditions not currently active, and the
              "excluded" field is true when the host names the feature
              in its "excludeFeatures" list. Exclusion overrides
              everything else: selecting the missing preconditions has
              no effect until the exclusion is lifted. The record
              reads the full preconditions table even in an evaluator
              that activates no contingent features on its own (see
              the "_hostSelectionsComputed" option), so the
              diagnostics survive there.
            '';
          };
        };

        config = let
          # The activation answer: prune, filter, and run the
          # fixpoint in one step.
          expanded =
            if hasImplicationsLib
            then
              flakeLib.resolveActivation {
                inherit implications knownByRole selected;
                preconditions = applicablePreconditions;
                excluded = {
                  profiles = host.excludeProfiles;
                  features = host.excludeFeatures;
                };
              }
            else selected;
          activeProfiles = expanded.profiles;
          # Every name the walk reached: active features and expressed
          # interests together, since a precondition may cite either
          # kind. The two are published apart, so keep the union to
          # this scope.
          namesInEffect = expanded.features;
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
              missing = builtins.filter (r: !(builtins.elem r namesInEffect)) preconditions;
              excluded = builtins.elem name host.excludeFeatures;
            })
            (lib.filterAttrs (name: _: !(builtins.elem name namesInEffect))
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

    _computedHostSelections = mkOption {
      type = types.nullOr (
        types.submodule {
          options = {
            profiles = mkOption {
              type = types.listOf types.str;
              description = "The computed union of active profiles.";
            };
            features = mkOption {
              type = types.listOf types.str;
              description = "The computed union of active features.";
            };
          };
        }
      );
      readOnly = true;
      internal = true;
      description = ''
        The union of per-user activations, computed here from the
        "dotfiles.users" registry and assigned (at default
        priority) into "dotfiles.host.profiles" and
        "dotfiles.host.features", or null when the registry is
        empty or no implications library is available. Each user's
        activation resolves separately: the user's exclusions prune
        the implication graph, the activation fixpoint runs on that
        user's own selections against the full preconditions table,
        and a
        contingent feature activates exactly when that single
        user's resulting set satisfies all of its preconditions. The
        "_hostSelectionsComputed" computation below compares the
        host's selections against this record.

        This option is read-only and computed in place, so the
        module system rejects any other definition outright: no
        module can fabricate a value that would satisfy the
        selection-equality guard. What remains writable is the
        registry the computation reads: entries written into
        "dotfiles.users" flow through each user's nested evaluator
        and its full per-user assertions (which reject, for
        example, selecting a contingent feature by name), so
        fabricating selections that way defeats itself.
      '';
    };

    _hostSelectionsComputed = mkOption {
      type = types.bool;
      readOnly = true;
      internal = true;
      description = ''
        True when this evaluator's "dotfiles.host.profiles" and
        "dotfiles.host.features" lists carry exactly the union of
        per-user activations computed from the "dotfiles.users"
        registry (see the "_computedHostSelections" option above)
        and that registry is non-empty. With the selections
        precomputed, contingent features activated for a single user
        already appear in those lists, so the activation walk here
        must not activate any more of them (this evaluator's
        selections union different users' sets, and a conjunction
        across users is forbidden), and the assertion forbidding
        contingent names in "dotfiles.host.features" stands down in
        favor of the per-user evaluators' checks against the
        human-written lists. Both relaxations are safe only for the
        computed lists, so this flag holds only while the host's
        selections still equal the recorded union: a consumer
        overriding "dotfiles.host.profiles" or
        "dotfiles.host.features" directly reverts this evaluator to
        normal semantics: the assertion applies again and the
        activation walk uses the full preconditions table.
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

  # Diagnose exclusions that name a known item which is already
  # inactive once the other exclusions have been applied: the
  # exclusion has no effect and may be removed. For each candidate
  # name "n" in "excludeProfiles" (or "excludeFeatures"), recompute
  # the activation with "n" temporarily removed from its exclusion
  # list (but with all other exclusions still pruning the graph). If
  # "n" is absent from the resulting activation, it would not have
  # been active anyway, so listing it as excluded changes nothing.
  config = let
    hostLabel = toString host.name;
    sameNames = a: b:
      lib.sort lib.lessThan (lib.unique a) == lib.sort lib.lessThan (lib.unique b);
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
          inherit implications knownByRole selected;
          preconditions = applicablePreconditions;
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
        inherit implications knownByRole selected;
        preconditions = applicablePreconditions;
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
    mkWarning = role: option: name: ''
      Resolving host "${hostLabel}": ${option} entry "${name}" names a known ${role} that is already inactive in the resolved activation; the exclusion has no effect and may be removed.
    '';
  in {
    dotfiles._featureUniverse = lib.unique (
      config.dotfiles._knownFeatures ++ config.dotfiles._knownInterests
    );

    dotfiles._implications = implications;

    dotfiles._computedHostSelections = computedHostSelections;

    # Start the host's selections from the computed union. The
    # default priority lets a host file override either list, which
    # displaces the union and reverts this evaluator to normal
    # enforcement; see the "_hostSelectionsComputed" option's
    # description.
    dotfiles.host = lib.mkIf (computedHostSelections != null) {
      profiles = lib.mkDefault computedHostSelections.profiles;
      features = lib.mkDefault computedHostSelections.features;
    };

    # Trust the host's selections as the computed union only while
    # they still set-equal that record; see the
    # "_hostSelectionsComputed" option's description.
    dotfiles._hostSelectionsComputed = let
      computed = config.dotfiles._computedHostSelections;
    in
      computed
      != null
      && sameNames host.profiles computed.profiles
      && sameNames host.features computed.features;

    warnings = lib.seq _unprunedSideEffect (
      map (mkWarning "profile" "excludeProfiles") redundantProfiles
      ++ map (mkWarning "feature" "excludeFeatures") redundantFeatures
    );
  };
}
