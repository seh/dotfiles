# Activation substrate shared by Home Manager, nix-darwin, and NixOS
# configurations.
#
# This module declares the options that describe the host and the
# resolved activation record of the evaluator it runs in, plus the set
# of "known profile and feature names" that profile and feature
# modules advertise. It is imported by each of
# "flake.modules.homeManager.default", "flake.modules.darwin.default",
# and "flake.modules.nixos.default", so it evaluates once per
# evaluator: once for the machine in a system configuration, and once
# more inside each managed user's nested home-manager evaluator.
#
# Each evaluation resolves its own activation alone, from the
# "dotfiles.host" record that evaluator sees. In a system evaluator
# that record carries the machine's own selections, so a system-class
# body follows those alone and no user may configure the machine. In a
# managed user's evaluator it carries the machine's selections layered
# with that user's own (see the propagation module in
# "modules/lib/_constructors.nix"), so the machine provisions every
# user it manages and each user adds to that.
#
# It imports the "_users.nix" declaration module so that the
# "dotfiles.users" registry is visible wherever this module
# evaluates, including in the home-manager class, where the
# aggregator in "modules/home/default.nix" demands that it stay
# empty.
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
  inherit (import ./lib/_option-types.nix {inherit lib;}) setOfNames preconditionSet preconditionEntry;

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
    features = config.dotfiles._knownNames;
  };
  preconditions = config.dotfiles._featurePreconditions;
  implications =
    if hasImplicationsLib
    then
      flakeLib.implicationsFor {
        inherit platform;
        knownProfiles = config.dotfiles._knownProfiles;
        supportedPlatforms = config.dotfiles._supportedPlatforms;
        impliedEdges = config.dotfiles._impliedEdges;
      }
    else null;
  # The profiles, features, and interests this evaluator itself
  # selects. See the "dotfiles.host" submodule description for the
  # machine and per-user readings. The walk carries two roles, and its
  # "features" role spans every registered name, so the selected
  # interests fold in there beside the selected features: each kind
  # has its own authoring list, and activation treats them alike.
  selected = {
    inherit (host) profiles;
    features = host.features ++ host.interests;
  };
  # Keep each feature and interest list to the names its own kind's
  # registry advertises before the walk reads it. The role-mismatch
  # assertions in "modules/_assertions.nix" refuse a name written
  # under the other kind's list, and the walk agreeing with them keeps
  # a refused line from withholding anything: a feature's name in
  # "excludeInterests" or "forbidInterests" (or an interest's in the
  # feature lists) prunes nothing, exactly as the complaint says to
  # expect. The profile lists need no such filter, because the
  # profiles role prunes only profile vertices already.
  ofKind = registry: builtins.filter (name: builtins.elem name registry);
  # The exclusions in force for this evaluator's walk, each authoring
  # list first held to its own kind.
  exclusionsInForce = {
    profiles = host.excludeProfiles;
    features =
      ofKind config.dotfiles._knownFeatures host.excludeFeatures
      ++ ofKind config.dotfiles._knownInterests host.excludeInterests;
  };
  # Fold what "host.forbidProfiles", "host.forbidFeatures", and
  # "host.forbidInterests" forbid into this evaluator's own
  # exclusions: all three prune every walk before "resolveActivation",
  # so a forbidden name activates nowhere—the one exclusion no user
  # may undo. The host record conveys what they forbid and keeps it
  # intact when the propagation module in
  # "modules/lib/_constructors.nix" layers the machine's record with a
  # user's, so each user's own home environment respects it through
  # this same logic, with no extra wiring.
  withForbidden = excluded: {
    profiles = excluded.profiles ++ host.forbidProfiles;
    features =
      excluded.features
      ++ ofKind config.dotfiles._knownFeatures host.forbidFeatures
      ++ ofKind config.dotfiles._knownInterests host.forbidInterests;
  };
  # This evaluator's own activation, resolved coherently in one walk
  # from the selections in the "dotfiles.host" record. The walk runs
  # the full preconditions table over that one set, so a contingent
  # feature activates exactly when this evaluator's own activation
  # satisfies its preconditions. In a system evaluator the selections
  # are the machine's own; in a managed user's evaluator they are the
  # machine's layered with that user's own, and the two may satisfy a
  # contingent feature's preconditions jointly.
  ownActivation =
    if hasImplicationsLib
    then
      flakeLib.resolveActivation {
        inherit implications knownByRole platform selected preconditions;
        supportedPlatforms = config.dotfiles._supportedPlatforms;
        excluded = withForbidden exclusionsInForce;
      }
    else selected;
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
              Profile names this machine or user selects. Expansion
              starts from these selected names.
            '';
          };
          features = mkOption {
            type = types.listOf types.str;
            default = [];
            description = ''
              Feature names this machine or user selects directly
              (outside of any profile that would pull them in).
              Expansion starts from these selected names together with
              "profiles".
            '';
          };
          interests = mkOption {
            type = types.listOf types.str;
            default = [];
            description = ''
              Interest names this machine or user selects: the wants
              that guard contingent features, such as a programming
              language in use here. Expansion starts from these
              selected names together with "profiles" and "features".
              An interest carries no configuration of its own, so
              selecting one activates only the contingent features
              whose preconditions it completes. A feature name belongs
              in "features" instead; the two kinds are not
              interchangeable, and an assertion in
              "modules/_assertions.nix" rejects a name written under
              the wrong one.
            '';
          };
          excludeProfiles = mkOption {
            type = types.listOf types.str;
            default = [];
            description = ''
              Profile names its author deletes from its own
              implication graph before the activation walk. Both their
              out-edges (the dependencies they would advertise) and
              their in-edges (other profiles that target them) are
              removed, so anything reachable only through an excluded
              profile is automatically absent from the resolved
              activation. The machine's own entries withhold a profile
              from what the machine provisions its users, yet yield to
              a user who selects that same name; a user's own entries
              apply to that user alone. The list no user may undo is
              "forbidProfiles".
            '';
          };
          excludeFeatures = mkOption {
            type = types.listOf types.str;
            default = [];
            description = ''
              Feature names its author deletes from its own
              implication graph before the activation walk. A feature
              still reachable through a non-excluded path remains
              active; one reachable only through excluded vertices
              drops out. Excluding a contingent feature by name keeps
              it inactive even when its preconditions are all met. The
              machine's own entries withhold a feature from what the
              machine provisions its users, yet yield to a user who
              selects that same name; a user's own entries apply to
              that user alone. The list no user may undo is
              "forbidFeatures".
            '';
          };
          excludeInterests = mkOption {
            type = types.listOf types.str;
            default = [];
            description = ''
              Interest names its author deletes from its own
              implication graph before the activation walk. An
              interest still reachable through a non-excluded
              path—another interest that implies it, say—remains
              active; one reachable only through excluded vertices
              drops out. Withholding an interest withholds every
              contingent feature that has no other way to satisfy its
              preconditions. The machine's own entries withhold an
              interest from what the machine provisions its users, yet
              yield to a user who selects that same name; a user's own
              entries apply to that user alone. The list no user may
              undo is "forbidInterests".
            '';
          };
          forbidProfiles = mkOption {
            type = setOfNames {merge = "union";};
            default = [];
            description = ''
              Machine-wide forbidding: each named profile is pruned
              from every activation walk, the machine's own and every
              user's, so it activates nowhere and no user receives
              it—not even a user who selects that same name.
              Forbidding is the absolute counterpart to
              "excludeProfiles", which prunes only its author's own
              walk and, at the machine level, yields to a user's
              explicit selection.
            '';
          };
          forbidFeatures = mkOption {
            type = setOfNames {merge = "union";};
            default = [];
            description = ''
              Machine-wide forbidding: the walk prunes each feature
              listed here from every activation, the machine's own and
              every user's, so it activates nowhere and no user
              receives it—not even a user who selects that same name.
              Forbidding is the absolute counterpart to the
              "excludeFeatures" list, which prunes only its author's
              own walk and, at the machine level, yields to a user's
              explicit selection.
            '';
          };
          forbidInterests = mkOption {
            type = setOfNames {merge = "union";};
            default = [];
            description = ''
              Machine-wide forbidding: the walk prunes each interest
              listed here from every activation, the machine's own and
              every user's, so it activates nowhere and no user
              receives it—not even a user who selects that same name.
              Forbidding is the absolute counterpart to the
              "excludeInterests" list, which prunes only its author's
              own walk and, at the machine level, yields to a user's
              explicit selection.
            '';
          };
        };
      };
      default = {};
      description = ''
        The host record in force in this evaluator: the "profiles",
        "features", and "interests" it selects, the exclusions it
        applies to its own walk, and the machine-wide
        "forbidProfiles"/"forbidFeatures"/"forbidInterests" lists. In
        a system evaluator these fields carry the machine's own wants
        alone, standing alongside the users in "dotfiles.users" and
        absorbing nothing from them, so no user's selection configures
        the machine. In a managed user's nested home-manager evaluator
        the propagation module in "modules/lib/_constructors.nix"
        layers the machine's record with that user's own, so the
        machine provisions every user it manages and each user adds to
        that. On a standalone home-manager host the sole user is the
        machine, so these are that user's selections. Set by
        "lib.mkHome", "lib.mkDarwin", and "lib.mkNixOS" from the
        "host = {...}" argument; consumer modules may extend its lists
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
              Profiles in effect for this evaluator: the coherent
              activation resolved from "dotfiles.host", the walk from
              "host.profiles" with "host.excludeProfiles" deleted from
              the implication graph. In a system evaluator that is the
              machine's own selections; in a managed user's nested
              home-manager evaluator it is the machine's selections
              layered with that user's own.
            '';
          };
          activeFeatures = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            description = ''
              Features in effect for this evaluator: the coherent
              activation resolved from "dotfiles.host". The walk
              deletes this evaluator's exclusions from the implication
              graph, walks the remaining edges from the selected
              profiles, features, and interests, and activates every
              contingent feature whose preconditions
              the result meets; a feature reachable only through an
              excluded profile is automatically absent. These active
              features decide whether each feature's configuration for
              this evaluator's class applies, tested via "inEffect". A
              system-class body therefore follows the machine's own
              selections alone, and a user's home-class body follows
              the machine's selections layered with that user's own.
              An inactive feature contributes nothing, as though it
              were never defined.

              The walk includes interests as well as features, since a
              precondition may cite either kind. This list holds the
              features alone; see "expressedInterests" for the
              interests.
            '';
          };
          expressedInterests = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            description = ''
              Interests expressed by this evaluator, whether it
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
              Predicate answering whether a name is in effect for this
              evaluator: true for an active feature and for an
              expressed interest alike. Feature bodies consult this to
              decide whether their configuration applies.
            '';
          };
          inactiveProfiles = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            description = ''
              Profiles advertised via "dotfiles._knownProfiles" that
              are not active for this evaluator: absent from
              "activeProfiles", whatever keeps them out, whether
              nothing here selected them, an exclusion pruned them, or
              they are unsupported on the machine's platform. Exposed
              as a diagnostic aid.
            '';
          };
          inactiveFeatures = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            description = ''
              Features advertised via "dotfiles._knownFeatures" that
              are not active for this evaluator, whatever keeps them
              out: nothing here selected them, an exclusion pruned
              them, or the machine's platform does not support them.
              Exposed as a diagnostic aid: each entry is a name a
              selector could select, so the list leaves out the
              contingent features, which no selector may select; see
              "latentFeatures" for those.
            '';
          };
          unexpressedInterests = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            description = ''
              Interests advertised via "dotfiles._knownInterests" that
              this evaluator does not express, whether nothing
              expressed them, an exclusion pruned them, or the
              machine's platform does not support them. Exposed as a
              diagnostic aid: each entry is an interest a selector
              could express, leaving the contingent features citing it
              latent.
            '';
          };
          latentFeatures = mkOption {
            type = types.attrsOf (
              types.submodule {
                options = {
                  preconditions = mkOption {
                    type = preconditionSet {};
                    description = ''
                      The feature's full preconditions list. Each
                      entry is a bare feature or interest name, or a
                      group "{ anyOf = [ "<name>" ... ]; }" satisfied
                      when any one member is active.
                    '';
                  };
                  missing = mkOption {
                    type = types.listOf preconditionEntry;
                    description = ''
                      The preconditions this evaluator's own coherent
                      activation does not satisfy: a bare name whose
                      feature or interest is inactive, or an "anyOf"
                      group none of whose members is active. A group
                      appears as its "{ anyOf = [ ... ]; }" record so
                      a reader sees the unsatisfied disjunction and
                      which alternatives it wanted; a satisfied entry
                      is absent.
                    '';
                  };
                  excluded = mkOption {
                    type = types.bool;
                    description = ''
                      True when this evaluator's own
                      "host.excludeFeatures" list names this feature:
                      the machine's own exclusions in a system
                      evaluator, and those in force for the user in a
                      managed user's evaluator.
                    '';
                  };
                };
              }
            );
            readOnly = true;
            description = ''
              Contingent features latent for this evaluator, keyed by
              name. A contingent feature already
              active here (present in "activeFeatures") is left out
              and never also reported latent; the entry for each of
              the rest carries the per-field reasons declared below.
              Each managed user's nested evaluator computes its own
              "latentFeatures" from the selections in force for that
              user.
            '';
          };
        };

        config = let
          activeProfiles = ownActivation.profiles;
          # Every name this evaluator's walk brought into effect:
          # active features and expressed interests together, since a
          # precondition may cite either kind. The two are published
          # apart, so the union stays a local binding.
          namesInEffect = ownActivation.features;
          isInterest = name: builtins.elem name config.dotfiles._knownInterests;
          contingentNames = builtins.attrNames config.dotfiles._featurePreconditions;
          # A precondition entry this evaluator's own activation does
          # not satisfy: a bare name neither active nor expressed, or
          # a group no member of which is either. A precondition may
          # cite a feature or an interest, so both tests read the
          # "namesInEffect" binding.
          entryUnmet = entry:
            if builtins.isString entry
            then !(builtins.elem entry namesInEffect)
            else !(lib.any (m: builtins.elem m namesInEffect) entry.anyOf);
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
              missing = builtins.filter entryUnmet preconditions;
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
        Folded in beside the known feature names for activation,
        exclusion, and preconditions, and kept apart so that
        kind-aware checks (such as the namespace-disjointness
        assertion in "modules/_assertions.nix") can tell interests
        from features.
      '';
    };

    _knownNames = mkOption {
      type = types.listOf types.str;
      readOnly = true;
      internal = true;
      description = ''
        The names a selection, an exclusion, or a precondition may
        cite: the known feature names and the known interest names,
        one list without duplicates. This module computes it from
        "dotfiles._knownFeatures" and "dotfiles._knownInterests",
        which stay apart so that kind-aware checks can tell interests
        from features. The two kinds stand together here because a
        host selects or excludes an interest, and a precondition may
        list it, exactly as with a feature.
      '';
    };

    _featureClasses = mkOption {
      type = types.attrsOf (types.listOf types.str);
      default = {};
      description = ''
        Per-feature module classes, keyed by feature name. Each value
        lists the classes ("homeManager", "nixDarwin", "nixOS") that
        register a body for that feature. Each class aggregator
        mirrors it from the flake-level "dotfiles.featureClasses"
        registry. The mixed-body assertion in
        "modules/_assertions.nix" reads it to reject a feature
        registering both a home body and a system body. A feature
        registered by name alone, with no bodies, is absent.
      '';
    };

    _featurePreconditions = mkOption {
      type = types.attrsOf (preconditionSet {});
      default = {};
      description = ''
        Per-feature preconditions, keyed by feature name. Each value
        is a conjunction of entries that must all be satisfied before
        the keyed contingent feature activates. An entry is either a
        bare feature or interest name (satisfied when that name is
        active) or a group "{ anyOf = [ "<name>" ... ]; }" (satisfied
        when any one member is active). Mirrored from the flake-level
        "dotfiles.featurePreconditions" registry by each class
        aggregator. Consulted by the activation fixpoint in
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

    _profileClasses = mkOption {
      type = types.attrsOf (types.listOf types.str);
      default = {};
      description = ''
        Per-profile module classes, keyed by profile name; each value
        names the classes ("homeManager", "nixDarwin", "nixOS") that
        register a body for that profile. Mirrored from the
        flake-level "dotfiles.profileClasses" registry by each class
        aggregator. Consulted by the assertion in
        "modules/_assertions.nix" that rejects a user selecting a
        profile which configures the machine alone. A profile
        registered by name alone, with no bodies, is absent.
      '';
    };

    _supportedPlatforms = mkOption {
      type = types.attrsOf (types.listOf types.str);
      default = {};
      description = ''
        Per-name platform support, keyed by profile or feature name.
        Each value lists the platforms on which that name may
        activate. Each class aggregator mirrors it from the
        flake-level "dotfiles.supportedPlatforms" registry. The
        implication-graph computation reads it to drop an unsupported
        name from every target list. The platform-support assertion in
        "modules/_assertions.nix" reads it as well.
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

  # Diagnose exclusions that name a known item this evaluator's own
  # selections do not activate: the exclusion has no effect here and
  # may be removed. For each candidate name "n" in "excludeProfiles"
  # (or "excludeFeatures", or "excludeInterests"), recompute this
  # evaluator's own activation with "n" temporarily removed from its
  # exclusion list (but with all other exclusions still pruning the
  # graph). If "n" is absent from the resulting activation, it would
  # not have been active anyway, so listing it as excluded changes
  # nothing. The judgment stays within one evaluator: the machine's
  # own selections in a system evaluator, and the machine's layered
  # with the user's in a managed user's evaluator, so each evaluator
  # reports only the exclusions idle there.
  config = let
    hostLabel = toString host.name;
    # The unpruned walk also forces the dangling-edge check inside
    # "expandClosure" and the precondition-cycle check inside
    # "expandActivation" to run against the full tables. Pruning
    # could otherwise hide a typo in an excluded profile's
    # adjacency list, or a cycle behind an excluded member.
    _unprunedSideEffect =
      if hasImplicationsLib
      then
        flakeLib.resolveActivation {
          inherit implications knownByRole platform selected preconditions;
          supportedPlatforms = config.dotfiles._supportedPlatforms;
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
      withoutSelf =
        exclusionsInForce
        // {
          ${role} = lib.filter (m: m != name) exclusionsInForce.${role};
        };
      activation = flakeLib.resolveActivation {
        inherit implications knownByRole platform selected preconditions;
        supportedPlatforms = config.dotfiles._supportedPlatforms;
        excluded = withoutSelf;
      };
    in
      !(builtins.elem name (activation.${role} or []));
    redundantOf = role: known: excluded:
      if hasImplicationsLib
      then builtins.filter (n: builtins.elem n known && isRedundant role n) excluded
      else [];
    # Each exclusion list is judged against its own kind's registry:
    # "excludeFeatures" against the known features and
    # "excludeInterests" against the known interests, matching the
    # walk, which holds each list to its own kind. A name written
    # under the wrong list is a kind mismatch, which the assertions in
    # "modules/_assertions.nix" reject outright and the walk ignores.
    redundantProfiles = redundantOf "profiles" config.dotfiles._knownProfiles host.excludeProfiles;
    redundantFeatures = redundantOf "features" config.dotfiles._knownFeatures host.excludeFeatures;
    redundantInterests = redundantOf "features" config.dotfiles._knownInterests host.excludeInterests;
    # The machine-wide forbid option each kind answers to, named in
    # the warning below as the absolute alternative to an exclusion.
    forbidOptions = {
      profile = "forbidProfiles";
      feature = "forbidFeatures";
      interest = "forbidInterests";
    };
    # The kind with an indefinite article that fits it, since
    # "interest" takes "an" where "profile" and "feature" take "a".
    describeKind = role:
      if role == "interest"
      then "an interest"
      else "a ${role}";
    mkWarning = role: option: name: let
      forbidOption = forbidOptions.${role};
    in ''
      Resolving host "${hostLabel}": ${option} entry "${name}" names a known ${role} that the selections in force here do not activate; the exclusion has no effect on this configuration and may be removed. A machine-wide forbid list that keeps ${describeKind role} inactive for every walk, the machine's own and every user's, is spelled "dotfiles.host.${forbidOption}".
    '';
  in {
    dotfiles._knownNames = lib.unique (
      config.dotfiles._knownFeatures ++ config.dotfiles._knownInterests
    );

    warnings = lib.seq _unprunedSideEffect (
      map (mkWarning "profile" "excludeProfiles") redundantProfiles
      ++ map (mkWarning "feature" "excludeFeatures") redundantFeatures
      ++ map (mkWarning "interest" "excludeInterests") redundantInterests
    );
  };
}
