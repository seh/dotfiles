# Activation substrate shared by Home Manager, nix-darwin, and NixOS
# configurations.
#
# This module declares the options that describe the host and the
# resolved activation record of the evaluator it runs in, plus the set
# of known feature and interest names that the imported modules
# advertise. It is imported by each of the
# "flake.modules.homeManager.default", "flake.modules.darwin.default",
# and "flake.modules.nixos.default" modules, so it evaluates once per
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
  known = config.dotfiles._knownNames;
  preconditions = config.dotfiles._featurePreconditions;
  implications =
    if hasImplicationsLib
    then
      flakeLib.implicationsFor {
        inherit platform preconditions;
        supportedPlatforms = config.dotfiles._supportedPlatforms;
        impliedEdges = config.dotfiles._impliedEdges;
        knownFeatures = config.dotfiles._knownFeatures;
        featureClasses = config.dotfiles._featureClasses;
      }
    else null;
  # The features and interests this evaluator itself selects. See the
  # "dotfiles.host" submodule description for the machine and per-user
  # readings. The walk carries one list spanning every registered
  # name, so the selected interests join the selected features there:
  # each kind has its own authoring list, and activation treats them
  # alike.
  selected = host.features ++ host.interests;
  # Keep each feature and interest list to the names its own kind's
  # registry advertises before the walk reads it. The role-mismatch
  # assertions in the "modules/_assertions.nix" file refuse a name
  # written under the other kind's list, and the walk agreeing with
  # them keeps a refused line from withholding anything: a feature's
  # name in the "excludeInterests" or "forbidInterests" list (or an
  # interest's in the feature lists) prunes nothing, exactly as the
  # complaint says to expect.
  ofKind = registry: builtins.filter (name: builtins.elem name registry);
  # The exclusions in force for this evaluator's walk, each authoring
  # list first held to its own kind.
  exclusionsInForce =
    ofKind config.dotfiles._knownFeatures host.excludeFeatures
    ++ ofKind config.dotfiles._knownInterests host.excludeInterests;
  # Fold what "host.forbidFeatures" and "host.forbidInterests" forbid
  # into this evaluator's own exclusions: both prune every walk before
  # the "resolveActivation" function, so a forbidden name activates
  # nowhere—the one exclusion no user may undo. The host record
  # conveys what they forbid and keeps it intact when the propagation
  # module in the "modules/lib/_constructors.nix" file layers the
  # machine's record with a user's, so each user's own home
  # environment respects it through this same logic, with no extra
  # wiring.
  withForbidden = excluded:
    excluded
    ++ ofKind config.dotfiles._knownFeatures
    (host.forbidFeatures ++ config.dotfiles._machineForbidFeatures)
    ++ ofKind config.dotfiles._knownInterests
    (host.forbidInterests ++ config.dotfiles._machineForbidInterests);
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
        inherit implications known platform selected preconditions;
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
          features = mkOption {
            type = types.listOf types.str;
            default = [];
            description = ''
              The features this machine or user selects, from the
              finest single concern to a bundle that only brings
              others along. Expansion starts from these selected names
              together with the "interests" list.
            '';
          };
          interests = mkOption {
            type = types.listOf types.str;
            default = [];
            description = ''
              Interest names this machine or user selects: the wants
              that guard contingent features, such as a programming
              language in use here. Expansion starts from these
              selected names together with "features". An interest
              carries no configuration of its own, so selecting one
              activates only the contingent features whose
              preconditions it completes. A feature name belongs in
              "features" instead; the two kinds are not
              interchangeable, and an assertion in
              "modules/_assertions.nix" rejects a name written under
              the wrong one.
            '';
          };
          excludeFeatures = mkOption {
            type = types.listOf types.str;
            default = [];
            description = ''
              Feature names its author deletes from its own
              implication graph before the activation walk. Both their
              out-edges
              (the features they would bring along) and their in-edges
              (the features that target them) are removed, so a
              feature still reachable through a non-excluded path
              remains active while one reachable only through excluded
              vertices drops out. Excluding a contingent feature by
              name keeps it inactive even when its preconditions are
              all met. The machine's own entries withhold a feature
              from what the machine provisions its users, yet yield to
              a user who selects that same name; a user's own entries
              apply to that user alone. The list no user may undo is
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
        The host record this evaluator acts on: the names it selects
        under its "features" and "interests" lists, the exclusions it
        applies to its own walk, and the machine-wide
        "forbidFeatures"/"forbidInterests" lists. In a system
        evaluator these fields hold the machine's own wants alone,
        standing alongside the users in the "dotfiles.users" registry
        and absorbing nothing from them, so no user's selection
        configures the machine. In a managed user's nested
        home-manager evaluator the propagation module in the
        "modules/lib/_constructors.nix" file layers the machine's
        record with that user's own, so the machine provisions every
        user it manages and each user adds to that. On a host that
        home-manager alone manages, the sole user is the machine, so
        these are that user's selections. The "lib.mkHome",
        "lib.mkDarwin", and "lib.mkNixOS" constructors fill it from
        the "host = {...}" argument; consumer modules may extend its
        lists through the module system's append-merge.
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
          activeFeatures = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            description = ''
              Features in effect for this evaluator: the coherent
              activation resolved from "dotfiles.host". The walk
              deletes this evaluator's exclusions from the implication
              graph, walks the remaining edges from the selected
              features and interests, and activates every contingent
              feature whose preconditions the result meets; a feature
              reachable only through an excluded bundle is
              automatically absent. These active features decide
              whether each feature's configuration for this
              evaluator's class applies, tested via "inEffect". A
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
          # Every name this evaluator's walk brought into effect:
          # active features and expressed interests together, since a
          # precondition may cite either kind. The two are published
          # apart, so the union stays a local binding.
          namesInEffect = ownActivation;
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
          inherit platform;
          activeFeatures = builtins.filter (name: !(isInterest name)) namesInEffect;
          expressedInterests = builtins.filter isInterest namesInEffect;
          inEffect = name: builtins.elem name namesInEffect;
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

    # "knownFeatures" and "knownInterests" are intentionally
    # flake-wide registries, not per-class. A name advertised by any
    # module in any class (home-manager, darwin, or nixos) is accepted
    # in any host's selections regardless of that host's class.
    #
    # This arrangement lets a concept like "development machine" be
    # selected once at the host level and hook-able by any class whose
    # behavior is appropriate, implemented differently as each class
    # sees fit. Today the "development" feature has only a
    # home-manager implementation in the
    # "modules/home/features/development/default.nix" file, but the
    # same name can be selected on a NixOS or darwin host; activation
    # picks up whichever class-specific feature files exist for that
    # name and does nothing in classes where no such file does.
    #
    # Assertion consequence: typos are caught (a misspelled feature or
    # interest name fails the unknown-name check in the
    # "modules/_assertions.nix" file), but cross-class selections are
    # accepted as intended.
    # What the machine forbids, as the propagation module in the
    # "modules/lib/_constructors.nix" file writes it into each managed
    # user's evaluator. The walk prunes these beside the
    # "dotfiles.host.forbidFeatures" and "dotfiles.host.forbidInterests"
    # lists, so forbidding survives a module inside a user's own home
    # configuration rewriting that record. Both stay empty where
    # nothing propagates them—the machine's own evaluator, and a home
    # configuration nobody else builds—since there the two "host"
    # lists are the only source and their author is the one forbidding.
    #
    # These do not use the module system's "readOnly" flag, which
    # would refuse a second definition and so refuse a user's attempt
    # outright. That flag counts an option's default among its
    # definitions, so an option carrying both a default and one
    # definition already trips it, and these need both: the machine
    # writes them for a managed user, and nothing writes them
    # elsewhere. The "forbidOverriddenAssertion" assertion in the
    # "modules/_assertions.nix" file reports the attempt instead.
    _machineForbidFeatures = mkOption {
      type = types.listOf types.str;
      default = [];
      internal = true;
      description = ''
        The features the machine forbids, as written into a managed
        user's evaluator, which the walk prunes beside that user's own
        copy of the "dotfiles.host.forbidFeatures" list.
      '';
    };
    _machineForbidInterests = mkOption {
      type = types.listOf types.str;
      default = [];
      internal = true;
      description = ''
        The interests the machine forbids, as written into a managed
        user's evaluator, which the walk prunes beside that user's own
        copy of the "dotfiles.host.forbidInterests" list.
      '';
    };
    _knownFeatures = mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        Feature names that imported feature modules declare they
        respond to. Accumulated via "listOf"'s append-merge semantics.
        Used to catch typos in a host's selected "features" list, to
        diagnose role mismatches, and by "flake.lib.implicationsFor"
        as the full set of names from which it computes the "all"
        feature's targets.
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
        registry. The mixed-body assertion in the
        "modules/_assertions.nix" file reads it to reject a feature
        registering both a home body and a system body, and the
        "flake.lib.implicationsFor" function reads the keys alone to
        keep the computed "all" feature on the body-less bundles. A
        feature registered by name alone, with no bodies, is absent.
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
        "flake.lib.expandActivation", by the assertions in
        "modules/_assertions.nix", and by "flake.lib.implicationsFor",
        which reads the keys alone to keep the computed "all" feature
        off every contingent feature.
      '';
    };

    _impliedEdges = mkOption {
      type = types.attrsOf (types.listOf types.raw);
      default = {};
      description = ''
        Per-source implied edges, keyed by source feature or interest
        name; each value is that source's "implies" list. Mirrored
        from the flake-level "dotfiles.impliedEdges" registry by each
        class aggregator. Passed to "flake.lib.implicationsFor", which
        assembles the implication graph from these co-located
        declarations.
      '';
    };

    _supportedPlatforms = mkOption {
      type = types.attrsOf (types.listOf types.str);
      default = {};
      description = ''
        Per-name platform support, keyed by feature name. Each value
        lists the platforms on which that name may activate. Each
        class aggregator mirrors it from the flake-level
        "dotfiles.supportedPlatforms" registry. The implication-graph
        computation reads it to drop an unsupported name from every
        target list. The platform-support assertion in the
        "modules/_assertions.nix" file reads it as well.
      '';
    };

    _flakeLib = mkOption {
      type = with types; nullOr (lazyAttrsOf raw);
      default = null;
      internal = true;
      description = ''
        This flake's "flake.lib" record, which the activation
        computation here ("activeFeatures") reads. Populated by each
        class aggregator. Kept nullable so that direct instantiations
        of this module (e.g. for tests) remain possible without a
        flake-parts context.
      '';
    };
  };

  # Diagnose exclusions that name a known item this evaluator's own
  # selections do not activate: the exclusion has no effect here and
  # may be removed. For each candidate name "n" in "excludeFeatures"
  # (or "excludeInterests"), recompute this evaluator's own activation
  # with "n" temporarily removed from its exclusion list (but with all
  # other exclusions still pruning the graph). If "n" is absent from
  # the resulting activation, it would not have been active anyway, so
  # listing it as excluded changes nothing. The judgment stays within
  # one evaluator: the machine's own selections in a system evaluator,
  # and the machine's layered with the user's in a managed user's
  # evaluator, so each evaluator reports only the exclusions idle
  # there.
  config = let
    hostLabel = toString host.name;
    # The unpruned walk also forces the dangling-edge check inside
    # "expandClosure" and the precondition-cycle check inside
    # "expandActivation" to run against the full tables. Pruning
    # could otherwise hide a typo in an excluded feature's
    # adjacency list, or a cycle behind an excluded member.
    _unprunedSideEffect =
      if hasImplicationsLib
      then
        flakeLib.resolveActivation {
          inherit implications known platform selected preconditions;
          supportedPlatforms = config.dotfiles._supportedPlatforms;
        }
      else null;
    # Redundancy test: an excluded name "n" is redundant when, with
    # "n" removed from the exclusion list (but every other exclusion
    # still pruning), the resulting activation does not contain "n"
    # anyway. The test runs the full fixpoint: an exclusion
    # suppressing a contingent feature that would otherwise activate
    # has real effect, and a walk that never activates contingent
    # features would misreport it as removable.
    # The recomputation removes every exclusion entry bearing the
    # candidate name and then folds the machine-wide forbid lists back
    # in. Those lists must stay: dropping the name from them as well
    # would let a forbidden name activate here and earn the verdict
    # "this exclusion matters", when forbidding in fact keeps it
    # inactive whatever the exclusion says. Removing every entry at
    # once errs only toward silence: where two of the exclusions in
    # force here share the name, the recomputation can activate it and
    # stay quiet about a line whose removal alone would have changed
    # nothing.
    isRedundant = name: let
      activation = flakeLib.resolveActivation {
        inherit implications known platform selected preconditions;
        supportedPlatforms = config.dotfiles._supportedPlatforms;
        excluded = withForbidden (lib.filter (m: m != name) exclusionsInForce);
      };
    in
      !(builtins.elem name activation);
    redundantOf = registry: excluded:
      if hasImplicationsLib
      then builtins.filter (n: builtins.elem n registry && isRedundant n) excluded
      else [];
    # Each exclusion list is judged against its own kind's registry:
    # "excludeFeatures" against the known features and
    # "excludeInterests" against the known interests, matching the
    # walk, which holds each list to its own kind. A name written
    # under the wrong list is a kind mismatch, which the assertions in
    # "modules/_assertions.nix" reject outright and the walk ignores.
    redundantFeatures = redundantOf config.dotfiles._knownFeatures host.excludeFeatures;
    redundantInterests = redundantOf config.dotfiles._knownInterests host.excludeInterests;
    # The machine-wide forbid option each kind answers to, named in
    # the warning below as the absolute alternative to an exclusion.
    forbidOptions = {
      feature = "forbidFeatures";
      interest = "forbidInterests";
    };
    # The kind with an indefinite article that fits it, since
    # "interest" takes "an" where "feature" takes "a".
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
      map (mkWarning "feature" "excludeFeatures") redundantFeatures
      ++ map (mkWarning "interest" "excludeInterests") redundantInterests
    );
  };
}
