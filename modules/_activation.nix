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
# that record holds the machine's own selections, so a system-class
# body follows those alone and no user may configure the machine. In a
# managed user's evaluator it holds the machine's selections layered
# with that user's own (see the propagation module in
# "modules/lib/_constructors.nix"), so the machine provisions every
# user it manages and each user adds to that.
#
# It imports the "_users.nix" declaration module so that the
# "dotfiles.users" registry is visible wherever this module evaluates,
# including in the home-manager class, where the aggregator in
# "modules/home/default.nix" demands that it stay empty.
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
  inherit (import ./lib/_diagnostics.nix) describeContradiction describeHost describeForbiddenSelection;

  # Detect the host's platform from the evaluating package set. Every
  # real evaluator (Home Manager, nix-darwin, NixOS) provides one, so
  # the detected value is authoritative; host records have no platform
  # attribute. Null arises only in a bare instantiation of this module
  # without a package set.
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
  # readings. The walk keeps one list spanning every registered name,
  # so the selected interests join the selected features there: each
  # kind has its own authoring list, and activation treats them alike.
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
  # The four lists this evaluator writes on its own account. Every
  # evaluator but a managed user's nested one writes the "host" lists
  # directly, so its own lists are those; for a managed user the
  # propagation module in the "modules/lib/_constructors.nix" file
  # records that user's own four before layering them with the
  # machine's, since the layered result cannot tell a user declining
  # what the machine provides from a user contradicting itself.
  ownOr = recorded: fallback:
    if recorded != null
    then recorded
    else fallback;
  ownFeatures = ownOr config.dotfiles._ownFeatures host.features;
  ownInterests = ownOr config.dotfiles._ownInterests host.interests;
  ownExcludeFeatures = ownOr config.dotfiles._ownExcludeFeatures host.excludeFeatures;
  ownExcludeInterests = ownOr config.dotfiles._ownExcludeInterests host.excludeInterests;
  # Where this evaluator's own lists live, for a diagnostic to cite
  # the line its reader would edit. The propagation module supplies a
  # managed user's path, since a user may rename their identity and so
  # the identity is no attribute key.
  ownListPrefix = ownOr config.dotfiles._ownListPrefix "dotfiles.host";
  # A managed user's evaluator, where only the machine forbids names,
  # never this user. Selecting a name the machine forbids earns a
  # warning from the propagation module, so the report below on a name
  # selected and forbidden together stays with the machine that
  # authored both lines.
  managedUser = config.dotfiles._ownFeatures != null;
  # A machine that provisions users, gated the same way as the
  # "contingentExclusionAssertion" assertion in the
  # "modules/_assertions.nix" file: a managed user's nested evaluator
  # and a standalone home configuration both see the "dotfiles.users"
  # registry empty.
  provisionsUsers = config.dotfiles.users != {};

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
              declares no configuration of its own, so selecting one
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
              The features its author deletes from its own implication
              graph before the activation walk. The walk drops each
              excluded vertex with its out-edges (the features it
              would bring along) and its in-edges (the edges that
              target it). A feature still reachable through a
              non-excluded path stays active; one reachable only
              through excluded vertices drops out. The machine's own
              entries withhold a feature from what the machine
              provisions its users, yet yield to a user who selects
              that same name; a user's own entries apply to that user
              alone. The one list no user may undo is
              "forbidFeatures". A user may exclude a contingent
              feature, and that exclusion holds like any other. A
              machine that provisions users may not: no user may
              select a contingent feature, so no user could opt back
              in. The "contingentExclusionAssertion" assertion in the
              "modules/_assertions.nix" file rejects such an entry and
              points instead to "forbidFeatures", the list that
              withholds a name from every selector outright.
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
              The host's platform: the Nixpkgs system identifier (e.g.
              "aarch64-darwin") detected from the evaluating package
              set, or null when this module is instantiated without
              one. Detection is the only source; host records have no
              platform attribute.
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
              interest declares no configuration of its own;
              expressing one only completes the preconditions of the
              contingent features citing it. Declaring that an
              interest exists, via the "mkInterest" function, does not
              express it.
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
                  excludedBy = mkOption {
                    type = types.nullOr (
                      types.enum [
                        "machine"
                        "own"
                      ]
                    );
                    description = ''
                      Whose exclusion holds this feature, or null
                      where none does. "own" means the
                      "excludeFeatures" list this evaluator writes for
                      itself; "machine" means the machine's own
                      "excludeFeatures" list, arriving at a managed
                      user through the propagation module. Only the
                      feature list of either author counts: the walk
                      holds each exclusion list to its own kind, so a
                      feature's name written into an interest list
                      withholds nothing and earns a kind-mismatch
                      complaint from the assertions instead. The
                      "contingentExclusionAssertion" assertion in the
                      "modules/_assertions.nix" file rejects the
                      "machine" case, but that assertion guards a
                      build, and a system configuration's record
                      answers reads without one. (A home configuration
                      that manages nobody else forces its assertions
                      on any read.) A report consulted before a
                      successful build can therefore still meet the
                      "machine" case, and it must say so rather than
                      blame the user.
                    '';
                  };
                  unsupportedOn = mkOption {
                    type = types.nullOr (types.listOf types.str);
                    description = ''
                      The platforms this feature declares, where the
                      host's platform is not among them, and null
                      where the feature declares none or the host runs
                      one it declares. Preconditions cannot bring such
                      a feature into effect however many of them hold,
                      since the "expandActivation" function in the
                      "modules/lib/_implications.nix" file withholds
                      it there, so the report states the requirement
                      rather than inviting work that cannot pay off.
                      Selecting one directly would activate it, which
                      the "platformSupportAssertion" assertion
                      refuses, so no valid configuration does it, and
                      no one may select a contingent feature at all.
                    '';
                  };
                  forbidden = mkOption {
                    type = types.bool;
                    description = ''
                      True when the machine-wide "forbidFeatures" list
                      holds this feature. Only that list counts: the
                      walk lets "forbidFeatures" forbid features alone
                      and "forbidInterests" forbid interests alone, so
                      a feature's name in the "forbidInterests" list
                      is a kind-mismatch the assertions refuse and the
                      walk ignores, never a prohibition. The machine's
                      forbidding passes through to every managed user
                      untouched, so a user inherits everything it
                      forbids. The field can still differ between two
                      evaluators of one configuration: the
                      "forbidFeatures" option merges its definitions
                      as their union, so a module inside one user's
                      home configuration adds a name to that user's
                      copy alone. An exclusion never sets it; the
                      "excludedBy" field beside this one reports
                      exclusions.
                    '';
                  };
                };
              }
            );
            readOnly = true;
            description = ''
              Contingent features latent for this evaluator, keyed by
              name. A contingent feature already active here (present
              in the "activeFeatures" field) never appears; the entry
              for each of the rest holds the per-field reasons
              declared below. Each managed user's nested evaluator
              computes its own "latentFeatures" field from the
              selections that hold for that user.

              A flake that imports the
              "inputs.dotfiles.modules.flake.latentFeatures" module
              gathers these records across every evaluator of every
              configuration it publishes and presents them as its own
              "latentFeatures" output. The "modules/lib/_reports.nix"
              file decides which of these entries that output admits
              and computes the report, and nothing evaluates it until
              someone asks.
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
              # Only the "excludeFeatures" lists can hold a feature
              # back, since the walk holds each exclusion list to its
              # own kind. An evaluator's own exclusion outranks a
              # machine's, because removing the machine's would leave
              # the evaluator's own still standing, so it is the one a
              # reader can act on.
              excludedBy =
                if builtins.elem name ownExcludeFeatures
                then "own"
                else if builtins.elem name host.excludeFeatures
                then "machine"
                else null;
              forbidden = builtins.elem name host.forbidFeatures;
              unsupportedOn = let
                declared = config.dotfiles._supportedPlatforms.${name} or null;
              in
                if declared == null || (platform != null && builtins.elem platform declared)
                then null
                else declared;
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

    _ownFeatures = mkOption {
      type = types.nullOr (types.listOf types.str);
      default = null;
      internal = true;
      description = ''
        The features this evaluator selects on its own account, as
        against the ones a machine passes down to it. Null in an
        evaluator that writes its own "host.features" list directly,
        which is every one but a managed user's nested evaluator; the
        propagation module in the "modules/lib/_constructors.nix" file
        sets it there to that user's own list. A diagnostic reporting
        a name its author both selects and excludes needs the author's
        own two lists, since the layered ones cannot tell a user
        declining what the machine provides from a user contradicting
        itself.
      '';
    };

    _ownInterests = mkOption {
      type = types.nullOr (types.listOf types.str);
      default = null;
      internal = true;
      description = ''
        The interests this evaluator expresses on its own account, the
        companion to the "_ownFeatures" option and null in the same
        places.
      '';
    };

    _ownListPrefix = mkOption {
      type = types.nullOr types.str;
      default = null;
      internal = true;
      description = ''
        The option path holding this evaluator's own four lists,
        without a trailing dot, for a diagnostic to cite the line its
        reader would edit. Null in an evaluator that writes the "host"
        lists directly, which then reads as the "dotfiles.host"
        record; the propagation module in the
        "modules/lib/_constructors.nix" file sets it for a managed
        user. That module supplies the path rather than a diagnostic
        deriving one, because a user may set an identity name
        differing from the attribute key their lists live under.
      '';
    };

    _ownExcludeFeatures = mkOption {
      type = types.nullOr (types.listOf types.str);
      default = null;
      internal = true;
      description = ''
        The features this evaluator excludes on its own account, as
        against the ones a machine passes down to it. Null in an
        evaluator that writes its own "host.excludeFeatures" list
        directly, which is every one but a managed user's nested
        evaluator; the propagation module in the
        "modules/lib/_constructors.nix" file sets it there to that
        user's own list. A diagnostic needs the two apart, since a
        user countermands a machine's exclusion by selecting the name
        and cannot countermand their own.
      '';
    };

    _ownExcludeInterests = mkOption {
      type = types.nullOr (types.listOf types.str);
      default = null;
      internal = true;
      description = ''
        The interests this evaluator excludes on its own account, the
        companion to the "_ownExcludeFeatures" option and null in the
        same places.
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

  # Three diagnostics, each addressed to whoever wrote the lines it
  # cites — the machine in a system evaluator, that user in a managed
  # user's.
  #
  # First, a name written into both a selection list and the matching
  # exclusion list. The exclusion holds and the selection has no
  # effect, so one of the two lines is not what its author meant.
  #
  # Second, a name a machine both selects and forbids. Forbidding
  # admits no exception, so that selection can never take effect.
  #
  # Third, an exclusion that changes nothing. A machine that
  # provisions users answers here only for an exclusion beside a name
  # it also forbids: its other exclusions exist to withhold names from
  # what it provisions, yielding to a user who asks, so judging them
  # by the machine's own activation would call a line doing exactly
  # its job useless. For a standalone home configuration and for a
  # managed user, whose exclusion lists bind their author alone, each
  # candidate name "n" among the exclusions its author wrote is judged
  # by what dropping that author's own line would change. Where an
  # entry the machine wrote survives beside it, the answer is nothing,
  # with no walk needed: the same set prunes either way. Elsewhere
  # every copy of "n" in force here is the author's own, so the
  # activation is recomputed with them dropped, every other exclusion
  # still pruning and everything the machine forbids still folded in;
  # "n" absent from the result means it would not have been active
  # anyway. An idle line may be removed, and the warning says so. A
  # name already reported under the first diagnostic is left out of
  # this one, since that message already asks its author to drop one
  # of two lines.
  config = let
    hostLabel = describeHost host.name;
    # The unpruned walk also forces the dangling-edge check inside
    # "expandClosure" and the precondition-cycle check inside
    # "expandActivation" to run against the full tables. Pruning could
    # otherwise hide a typo in an excluded feature's adjacency list,
    # or a cycle behind an excluded member.
    _unprunedSideEffect =
      if hasImplicationsLib
      then
        flakeLib.resolveActivation {
          inherit implications known platform selected preconditions;
          supportedPlatforms = config.dotfiles._supportedPlatforms;
        }
      else null;
    # A surviving exclusion of the name that the advised author did
    # not write: on a managed user's walk, the machine's. The layered
    # lists hold the author's own entries and the machine's surviving
    # ones together, so a count tells them apart even where both
    # authors wrote the name.
    othersExclude = own: name:
      builtins.length (builtins.filter (m: m == name) exclusionsInForce)
      > builtins.length (builtins.filter (m: m == name) own);
    # Redundancy test: an exclusion is idle when dropping the advised
    # author's own line would change nothing. Where an entry the
    # machine wrote also holds the name—surviving into a managed
    # user's walk because that user does not ask for it—no walk is
    # needed: the pruning set is the same with or without the author's
    # line. Only where every copy in force is the author's own does
    # the recomputation decide: drop them all, keep every other
    # exclusion pruning and everything the machine forbids folded in,
    # and call the line idle when the name stays inactive anyway. The
    # test runs the full fixpoint: an exclusion suppressing a
    # contingent feature that would otherwise activate has real
    # effect, and a walk that never activates contingent features
    # would misreport it as removable.
    #
    # What the machine forbids must stay: dropping the name from it as
    # well would let a forbidden name activate here and earn the
    # verdict "this exclusion matters", when forbidding in fact keeps
    # it inactive whatever the exclusion says. One case still errs
    # toward silence: an author who wrote the same name twice loses
    # both copies at once, so the recomputation can activate the name
    # and stay quiet about a line whose removal alone would have
    # changed nothing.
    isRedundant = own: name: let
      activation = flakeLib.resolveActivation {
        inherit implications known platform selected preconditions;
        supportedPlatforms = config.dotfiles._supportedPlatforms;
        excluded = withForbidden (lib.filter (m: m != name) exclusionsInForce);
      };
    in
      othersExclude own name || !(builtins.elem name activation);
    # A machine that provisions users skips the recomputation: its
    # exclusions exist to withhold names from what it provisions,
    # yielding to a user who asks for them, so whether the machine's
    # own walk would have activated the name is the wrong question,
    # and a line doing exactly its job would read as useless. Only an
    # exclusion beside a name it also forbids is reported there:
    # forbidding withholds the name from every walk and never yields,
    # so the exclusion adds nothing anywhere, and whether the machine
    # forbids the name decides that with no walk. Elsewhere the
    # recomputation decides.
    redundantOf = registry: forbidden: excluded:
      if !hasImplicationsLib
      then []
      else if provisionsUsers
      then builtins.filter (n: builtins.elem n registry && builtins.elem n forbidden) excluded
      else builtins.filter (n: builtins.elem n registry && isRedundant excluded n) excluded;
    # Each exclusion list is judged against its own kind's registry
    # and what it forbids: "excludeFeatures" against the known
    # features and "forbidFeatures", "excludeInterests" against the
    # known interests and "forbidInterests", matching the walk, which
    # holds each list to its own kind. A name written under the wrong
    # list is a kind mismatch, which the assertions in the
    # "modules/_assertions.nix" file reject outright and the walk
    # ignores.
    #
    # An entry its author also selects is reported as a contradiction
    # below, and that message already asks its author to drop one of
    # the two lines. Reporting the same entry a second time as idle
    # would answer a question nobody has yet: whether the exclusion
    # earns its place is moot while a selection beside it disagrees.
    redundantFeatures =
      lib.subtractLists contradictoryFeatures
      (redundantOf config.dotfiles._knownFeatures host.forbidFeatures ownExcludeFeatures);
    redundantInterests =
      lib.subtractLists contradictoryInterests
      (redundantOf config.dotfiles._knownInterests host.forbidInterests ownExcludeInterests);
    # What the machine forbids, keyed by kind.
    forbidOf = {
      feature = host.forbidFeatures;
      interest = host.forbidInterests;
    };
    forbidOptionOf = {
      feature = "forbidFeatures";
      interest = "forbidInterests";
    };
    # The exclusion list each kind's own entries live in, for the
    # machine's-entry test below.
    ownExclusionsOf = {
      feature = ownExcludeFeatures;
      interest = ownExcludeInterests;
    };
    # An idle exclusion reads one of three ways. Where the machine
    # already forbids the name, the exclusion adds nothing and what
    # the machine forbids is the reason; this is the only way a
    # machine that provisions users hears, since the "redundantOf"
    # function above holds such a machine to its forbid-shadowed
    # entries. Where the machine's own exclusion still withholds the
    # name here, this line repeats it and the machine's entry is the
    # reason. Elsewhere nothing here calls for the name at all, and
    # forbidding is worth mentioning as the way to hold it down for
    # every user rather than here alone.
    #
    # All three cite the exclusion list by its full path, since the
    # judged list is the one this machine or this user wrote, and a
    # bare option name would read as the machine's where a user cannot
    # edit it.
    mkWarning = role: option: name:
      if builtins.elem name forbidOf.${role}
      then ''
        Resolving ${hostLabel}: "${ownListPrefix}.${option}" lists "${name}", but "dotfiles.host.${forbidOptionOf.${role}}" already forbids that ${role} machine-wide, so the entry changes nothing and you can remove it. Forbidding keeps the name inactive in every activation walk, the machine's own and every user's.
      ''
      else if othersExclude ownExclusionsOf.${role} name
      then ''
        Resolving ${hostLabel}: "${ownListPrefix}.${option}" lists "${name}", but the machine's own "dotfiles.host.${option}" entry already withholds that ${role} here, so the entry changes nothing and you can remove it. The machine's entry yields only to a user who asks for the name, not to one who also excludes it.
      ''
      else ''
        Resolving ${hostLabel}: "${ownListPrefix}.${option}" lists "${name}", but nothing in this configuration turns that ${role} on, so the entry changes nothing and you can remove it. A machine-wide "dotfiles.host.${forbidOptionOf.${role}}" entry would keep the ${role} inactive in every activation walk, the machine's own and every user's.
      '';

    # Names written into both a selection list and the matching
    # exclusion list of the same author. Every machine and every user
    # answers for its own four lists, so this runs the same way for
    # both.
    bothIn = these: those: lib.unique (builtins.filter (name: builtins.elem name those) these);
    contradictoryFeatures = bothIn ownFeatures ownExcludeFeatures;
    contradictoryInterests = bothIn ownInterests ownExcludeInterests;
    mkContradiction = role: selectionOption: exclusionOption: name:
      describeContradiction {
        inherit hostLabel name;
        selectionOption = "${ownListPrefix}.${selectionOption}";
        exclusionOption = "${ownListPrefix}.${exclusionOption}";
        # A managed user hears about what the machine forbids from the
        # propagation module, which knows whose entry it is, so this
        # message stays on the two lines that user wrote and leaves
        # forbidding out.
        forbidOption =
          if !managedUser && builtins.elem name forbidOf.${role}
          then "dotfiles.host.${forbidOptionOf.${role}}"
          else null;
      };

    # Names a machine both selects and forbids. Forbidding is the
    # machine's alone, so only a machine answers for such a pair; a
    # managed user selecting a name the machine forbids hears instead
    # from the propagation module in the
    # "modules/lib/_constructors.nix" file, which knows whose entry it
    # is.
    #
    # A name the machine also excludes is left out here, because the
    # message above already states all three lines. Reporting it twice
    # would say "the two lines" twice about two different pairs.
    selectedAndForbidden = these: contradicted: role:
      lib.optionals (!managedUser)
      (lib.subtractLists contradicted (bothIn these forbidOf.${role}));
    selectedAndForbiddenFeatures = selectedAndForbidden ownFeatures contradictoryFeatures "feature";
    selectedAndForbiddenInterests = selectedAndForbidden ownInterests contradictoryInterests "interest";
    mkForbiddenSelection = role: selectionOption: name:
      describeForbiddenSelection {
        inherit hostLabel name;
        selectionOption = "${ownListPrefix}.${selectionOption}";
        forbidOption = "dotfiles.host.${forbidOptionOf.${role}}";
      };
  in {
    dotfiles._knownNames = lib.unique (
      config.dotfiles._knownFeatures ++ config.dotfiles._knownInterests
    );

    warnings = lib.seq _unprunedSideEffect (
      map (mkContradiction "feature" "features" "excludeFeatures") contradictoryFeatures
      ++ map (mkContradiction "interest" "interests" "excludeInterests") contradictoryInterests
      ++ map (mkForbiddenSelection "feature" "features") selectedAndForbiddenFeatures
      ++ map (mkForbiddenSelection "interest" "interests") selectedAndForbiddenInterests
      ++ map (mkWarning "feature" "excludeFeatures") redundantFeatures
      ++ map (mkWarning "interest" "excludeInterests") redundantInterests
    );
  };
}
