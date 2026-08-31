# Activation substrate shared by Home Manager, nix-darwin, and NixOS
# configurations.
#
# This module declares the options that describe the resolved
# activation record of the evaluator it runs in, plus the set of known
# feature and interest names that the imported modules advertise. It
# is imported by each of the
# "flake.modules.homeManager.default", "flake.modules.darwin.default",
# and "flake.modules.nixos.default" modules, so it evaluates once per
# evaluator: once for the machine in a system configuration, and once
# more inside each managed user's nested home-manager evaluator.
#
# Each evaluation resolves its own activation alone, from the
# "dotfiles.host" record that evaluator sees. In a system evaluator
# that record lists the machine's own selections, so a system-class
# body follows those alone and no user may configure the machine. In a
# managed user's evaluator it lists the machine's selections layered
# with that user's own (see the propagation module in the
# "modules/lib/_constructors.nix" file), so the machine provisions
# every user it manages and each user adds to that.
#
# This module reads the "dotfiles.host" record and declares it
# nowhere. Two other modules declare that option, both from the one
# shared type in the "modules/lib/_host-submodule.nix" file. The
# "modules/_host-option.nix" file declares the writable one, which a
# machine's configuration writes, and so does the one person a home
# configuration serves directly. The
# "modules/_provisioned-host-option.nix" file declares the read-only
# one, which a managed user's nested evaluator receives and the
# propagation module in the "modules/lib/_constructors.nix" file
# defines once for that user.
#
# The options below that mirror a flake-level registry, and the ones
# that state a managed user's own lists, take the module system's
# "readOnly" flag and declare no default. Whoever writes one writes it
# exactly once, and the module system stops the build on a second
# definition, whatever its priority. That flag counts a default among
# an option's definitions, which is why none of them declares one.
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
  inherit (import ./lib/_option-types.nix {inherit lib;}) preconditionSet preconditionEntry;
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
  # assertions in the "modules/_assertions.nix" file fail the build
  # for a name written under the other kind's list, and the walk
  # agreeing with them keeps such a line from withholding anything: a
  # feature's name in the "excludeInterests" or "forbidInterests" list
  # (or an interest's in the feature lists) prunes nothing, exactly as
  # the complaint says to expect.
  ofKind = registry: builtins.filter (name: builtins.elem name registry);
  # The exclusions in force for this evaluator's walk, each authoring
  # list first filtered to its own kind.
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
  # A machine that provisions users, behind the same condition as the
  # "contingentExclusionAssertion" assertion in the
  # "modules/_assertions.nix" file. Only the two system class
  # aggregators import the declaration of the "dotfiles.users"
  # registry, through the "modules/_host-users.nix" file. In a managed
  # user's nested evaluator, and in a home configuration serving one
  # person alone, the option is absent and this test reads the empty
  # attribute set.
  provisionsUsers = (config.dotfiles.users or {}) != {};

  # Fold what "host.forbidFeatures" and "host.forbidInterests" forbid
  # into this evaluator's own exclusions: both prune every walk before
  # the "resolveActivation" function, so a forbidden name activates
  # nowhere. No selection a user writes undoes it. The host record
  # conveys what they forbid and keeps it intact when the propagation
  # module in the "modules/lib/_constructors.nix" file layers the
  # machine's record with a user's, so each user's own home
  # environment respects it through this same logic, with no extra
  # wiring.
  withForbidden = excluded:
    excluded
    ++ ofKind config.dotfiles._knownFeatures host.forbidFeatures
    ++ ofKind config.dotfiles._knownInterests host.forbidInterests;
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
  # The activation these same selections would have brought with
  # nothing withheld. Subtracting the real activation from this gives
  # every feature the selections asked for that an exclusion or
  # forbidding removed, which the latent-feature report presents
  # beside the contingent features waiting on a precondition.
  unwithheldActivation =
    if hasImplicationsLib
    then
      flakeLib.resolveActivation {
        inherit implications known platform selected preconditions;
        supportedPlatforms = config.dotfiles._supportedPlatforms;
      }
    else selected;
  # Everything the implication graph places beyond one name, that name
  # included. Consulted for a withheld name to say which withholding
  # stands in front of another, so the report can attribute a loss
  # nobody wrote down to the entry that caused it.
  beyondName = name:
    if hasImplicationsLib
    then flakeLib.expandClosure implications known [name]
    else [name];
in {
  options.dotfiles = {
    _host = mkOption {
      type = types.submodule {
        options = {
          platform = mkOption {
            type = types.nullOr types.str;
            readOnly = true;
            description = ''
              The host's platform: the Nixpkgs system identifier (e.g.
              "aarch64-darwin") that this module detects from the
              evaluating package set, or null in a bare instantiation
              without one. Detection is the only source; host records
              have no platform attribute.
            '';
          };
          activeFeatures = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            description = ''
              Features in effect for this evaluator: the coherent
              activation resolved from the "dotfiles.host" record. The
              walk deletes this evaluator's exclusions from the
              implication graph, walks the remaining edges from the
              selected features and interests, and activates every
              contingent feature whose preconditions the result meets.
              A feature reachable only through an excluded bundle is
              automatically absent. These active features decide
              whether each feature's configuration for this
              evaluator's class applies, tested via the "inEffect"
              predicate. A system-class body therefore follows the
              machine's own selections alone, and a user's home-class
              body follows the machine's selections layered with that
              user's own. An inactive feature contributes nothing, as
              though it were never defined.

              The walk includes interests as well as features, since a
              precondition may cite either kind. This list contains
              the features alone; see the "expressedInterests" field
              for the interests.
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
              The known features not active for this evaluator,
              whatever keeps them out: nothing here selected them, an
              exclusion pruned them, or the machine's platform does
              not support them. The imported modules advertise these
              names through the "dotfiles._knownFeatures" option. This
              list serves diagnosis: each entry is a name a selector
              could select, so it leaves out the contingent features,
              which no selector may select; see the "latentFeatures"
              field for those.
            '';
          };
          unexpressedInterests = mkOption {
            type = types.listOf types.str;
            readOnly = true;
            description = ''
              The known interests this evaluator does not express,
              whether nothing expressed them, an exclusion pruned
              them, or the machine's platform does not support them.
              The imported modules advertise these names through the
              "dotfiles._knownInterests" option. This list serves
              diagnosis: each entry is an interest a selector could
              express, leaving the contingent features citing it
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
                      Whose exclusion withholds this feature, or null
                      where none does. "own" means the
                      "excludeFeatures" list this evaluator writes for
                      itself; "machine" means the machine's own
                      "excludeFeatures" list, which the propagation
                      module supplies to a managed user. Only the
                      feature list of either author counts: the walk
                      consults each exclusion list for its own kind
                      alone, so a feature's name written into an
                      interest list withholds nothing and earns a
                      kind-mismatch complaint from the assertions
                      instead. The "contingentExclusionAssertion"
                      assertion in the "modules/_assertions.nix" file
                      rejects the "machine" case, but that assertion
                      guards a build, and a system configuration's
                      record answers reads without one. (A home
                      configuration that manages nobody else forces
                      its assertions on any read.) A report consulted
                      before a successful build can therefore still
                      meet the "machine" case, and it must say so
                      rather than blame the user.
                    '';
                  };
                  unsupportedOn = mkOption {
                    type = types.nullOr (types.listOf types.str);
                    description = ''
                      The platforms this feature declares, where the
                      host's platform is not among them, and null
                      where the feature declares none or the host runs
                      one it declares. Preconditions cannot bring such
                      a feature into effect however many are
                      satisfied, since the "expandActivation" function
                      in the "modules/lib/_implications.nix" file
                      withholds it there, so the report states the
                      requirement rather than inviting work that
                      cannot pay off. Selecting one directly would
                      activate it, which the
                      "platformSupportAssertion" assertion fails the
                      build for such a selection, so no valid
                      configuration does it, and no one may select a
                      contingent feature at all.
                    '';
                  };
                  forbidden = mkOption {
                    type = types.bool;
                    description = ''
                      True when the machine-wide "forbidFeatures" list
                      contains this feature. Only that list counts:
                      the walk lets "forbidFeatures" forbid features
                      alone and "forbidInterests" forbid interests
                      alone, so a feature's name in the
                      "forbidInterests" list is a kind mismatch that
                      fails the build, and one the walk ignores rather
                      than treating as a prohibition. The machine's
                      forbidding applies unchanged to every managed
                      user, so a user inherits everything it forbids.
                      The field can still differ between two
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
              for each of the rest contains the per-field reasons
              declared below. Each managed user's nested evaluator
              computes its own "latentFeatures" field from the
              selections that apply to that user.

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
          withheldFeatures = mkOption {
            type = types.attrsOf (
              types.submodule {
                options = {
                  excludedBy = mkOption {
                    type = types.nullOr (
                      types.enum [
                        "machine"
                        "own"
                      ]
                    );
                    description = ''
                      Whose exclusion withholds this feature, or null
                      where none does. The two values mean what they
                      mean for the "latentFeatures" field's own
                      "excludedBy" field.
                    '';
                  };
                  forbidden = mkOption {
                    type = types.bool;
                    description = ''
                      True when "host.forbidFeatures" forbids this
                      feature itself.
                    '';
                  };
                  beyond = mkOption {
                    type = types.listOf types.str;
                    description = ''
                      The withheld features this one lies beyond,
                      empty where the author withheld this feature
                      itself. A feature with entries here is one
                      nobody wrote down: it becomes active only
                      through a feature some list contains, so
                      removing that entry is what brings this one
                      back.
                    '';
                  };
                };
              }
            );
            readOnly = true;
            description = ''
              Features this evaluator's selections asked for that the
              walk removed, keyed by name. A feature is here when the
              walk with nothing withheld activates it and the walk as
              it stands does not, so the entries are exactly what an
              exclusion or forbidding cost. A contingent feature never
              appears: the "latentFeatures" field beside this one
              accounts for those.

              The distinction the "beyond" field records is the one
              worth reading. A feature whose "beyond" list is empty is
              one its author withheld on purpose. A feature with
              entries there is a consequence its author did not write
              down, which is the case that would otherwise pass in
              silence.
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
              # Only the "excludeFeatures" lists can withhold a
              # feature, since the walk consults each exclusion list
              # for its own kind alone. An evaluator's own exclusion
              # outranks a machine's, because removing the machine's
              # would leave the evaluator's own still standing, so it
              # is the one a reader can act on.
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
          withheldFeatures = let
            # Features the selections asked for that the walk removed.
            # Contingent features stay out: they become active through
            # their preconditions rather than through a selection, and
            # the "latentFeatures" field accounts for them.
            names =
              builtins.filter (
                n: !(isInterest n) && !(builtins.elem n contingentNames)
              )
              (lib.subtractLists namesInEffect unwithheldActivation);
            heldBy = name:
              if builtins.elem name ownExcludeFeatures
              then "own"
              else if builtins.elem name host.excludeFeatures
              then "machine"
              else null;
            isForbidden = name: builtins.elem name host.forbidFeatures;
            # The withheld features an author's own lists contain.
            # Each of the rest becomes active only through one of
            # these, so citing them says which entry to edit.
            written =
              builtins.filter (n: heldBy n != null || isForbidden n) names;
          in
            lib.listToAttrs (
              map (name: {
                inherit name;
                value = {
                  excludedBy = heldBy name;
                  forbidden = isForbidden name;
                  beyond =
                    if heldBy name != null || isForbidden name
                    then []
                    else
                      builtins.filter (
                        w: w != name && builtins.elem name (beyondName w)
                      )
                      written;
                };
              })
              names
            );
        };
      };
      readOnly = true;
      default = {};
      description = ''
        The activation record this module computes from the
        "dotfiles.host" record, the implication graph, and the
        detected platform. Feature bodies consult it through the
        "inEffect" predicate; the assertions and the latent-feature
        report read the rest. This module alone defines its fields;
        nothing else may set them.
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
    _knownFeatures = mkOption {
      type = types.listOf types.str;
      readOnly = true;
      description = ''
        The names of every feature an imported module advertises. Each
        class aggregator mirrors it from the flake-level
        "dotfiles.knownFeatures" registry. The unknown-name assertions
        read it to catch typos in a host's selected "features" list.
        The role-mismatch assertions read it to catch a name filed
        under the wrong kind. The "flake.lib.implicationsFor" function
        reads it as the full set of names from which it computes the
        "all" feature's targets.
      '';
    };

    _knownInterests = mkOption {
      type = types.listOf types.str;
      readOnly = true;
      description = ''
        The interest names the imported modules declare via the
        "mkInterest" function. Each class aggregator mirrors it from
        the flake-level "dotfiles.knownInterests" registry.
        Activation, exclusion, and preconditions accept these names
        beside the known feature names, while the separate registry
        lets kind-aware checks (such as the namespace-disjointness
        assertion in the "modules/_assertions.nix" file) tell
        interests from features.
      '';
    };

    _knownNames = mkOption {
      type = types.listOf types.str;
      readOnly = true;
      internal = true;
      description = ''
        The names a selection, an exclusion, or a precondition may
        cite: the known feature names and the known interest names,
        one list without duplicates. This module computes it from the
        "dotfiles._knownFeatures" and "dotfiles._knownInterests"
        options, which stay apart so that kind-aware checks can tell
        interests from features. The two kinds stand together here
        because a host selects or excludes an interest, and a
        precondition may list it, exactly as with a feature.
      '';
    };

    _featureClasses = mkOption {
      type = types.attrsOf (types.listOf types.str);
      readOnly = true;
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
      readOnly = true;
      description = ''
        Per-feature preconditions, keyed by feature name. Each value
        is a conjunction of entries that must all be satisfied before
        the keyed contingent feature activates. An entry is either a
        bare feature or interest name (satisfied when that name is
        active) or a group "{ anyOf = [ "<name>" ... ]; }" (satisfied
        when any one member is active). Each class aggregator mirrors
        it from the flake-level "dotfiles.featurePreconditions"
        registry. The activation fixpoint in the
        "flake.lib.expandActivation" function reads it, the assertions
        in the "modules/_assertions.nix" file read it, and the
        "flake.lib.implicationsFor" function reads the keys alone to
        keep the computed "all" feature off every contingent feature.
      '';
    };

    _impliedEdges = mkOption {
      type = types.attrsOf (types.listOf types.raw);
      readOnly = true;
      description = ''
        Per-source implied edges, keyed by source feature or interest
        name. Each value is that source's "implies" list. Each class
        aggregator mirrors it from the flake-level
        "dotfiles.impliedEdges" registry, and the
        "flake.lib.implicationsFor" function assembles the implication
        graph from these co-located declarations.
      '';
    };

    _supportedPlatforms = mkOption {
      type = types.attrsOf (types.listOf types.str);
      readOnly = true;
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
      readOnly = true;
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
      readOnly = true;
      internal = true;
      description = ''
        The interests this evaluator expresses on its own account, the
        companion to the "_ownFeatures" option and null in the same
        places.
      '';
    };

    _ownListPrefix = mkOption {
      type = types.nullOr types.str;
      readOnly = true;
      internal = true;
      description = ''
        The option path declaring this evaluator's own four lists,
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
      readOnly = true;
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
      readOnly = true;
      internal = true;
      description = ''
        The interests this evaluator excludes on its own account, the
        companion to the "_ownExcludeFeatures" option and null in the
        same places.
      '';
    };

    _flakeLib = mkOption {
      type = with types; nullOr (lazyAttrsOf raw);
      readOnly = true;
      internal = true;
      description = ''
        This flake's "flake.lib" record, which the activation
        computation here (the "activeFeatures" field) reads. Each
        class aggregator fills it. It stays nullable so that a direct
        instantiation of this module (e.g. for tests) remains possible
        without a flake-parts context.
      '';
    };
  };

  # Three diagnostics, each addressed to whoever wrote the lines it
  # cites — the machine in a system evaluator, that user in a managed
  # user's.
  #
  # First, a name written into both a selection list and the matching
  # exclusion list. The exclusion applies and the selection has no
  # effect, so one of the two lines is not what its author meant.
  #
  # Second, a name a machine both selects and forbids. Forbidding
  # prunes the name from every walk, so that selection does not take
  # effect.
  #
  # Third, an exclusion that changes nothing. A machine that
  # provisions users answers here only for an exclusion beside a name
  # it also forbids: its other exclusions exist to withhold names from
  # what it provisions, yielding to a user who asks, so judging them
  # by the machine's own activation would call a line doing exactly
  # its job useless. For a home configuration built on its own and for
  # a managed user, whose exclusion lists bind their author alone,
  # each candidate name "n" among the exclusions its author wrote is
  # judged by what dropping that author's own line would change. Where
  # an entry the machine wrote survives beside it, the answer is
  # nothing, with no walk needed: the same set prunes either way.
  # Elsewhere every copy of "n" in force here is the author's own, so
  # the activation is recomputed with them dropped, every other
  # exclusion still pruning and everything the machine forbids still
  # folded in; "n" absent from the result means it would not have been
  # active anyway. An idle line may be removed, and the warning says
  # so. A name already reported under the first diagnostic is left out
  # of this one, since that message already asks its author to drop
  # one of two lines.
  config = let
    hostLabel = describeHost host.name;
    # The unpruned walk also forces the dangling-edge check inside the
    # "expandClosure" function and the precondition-cycle check inside
    # the "expandActivation" function to run against the full tables.
    # Pruning could otherwise hide a typo in an excluded feature's
    # adjacency list, or a cycle behind an excluded member.
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
    # lists contain the author's own entries and the machine's
    # surviving ones together, so a count tells them apart even where
    # both authors wrote the name.
    othersExclude = own: name:
      builtins.length (builtins.filter (m: m == name) exclusionsInForce)
      > builtins.length (builtins.filter (m: m == name) own);
    # Redundancy test: an exclusion is idle when dropping the advised
    # author's own line would change nothing. Where an entry the
    # machine wrote also contains the name—surviving into a managed
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
    # consults each list for its own kind alone. A name written under
    # the wrong list is a kind mismatch, which the assertions in the
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
    # function above limits such a machine to its forbid-shadowed
    # entries. Where the machine's own exclusion still withholds the
    # name here, this line repeats it and the machine's entry is the
    # reason. Elsewhere nothing here calls for the name at all, and
    # forbidding is worth mentioning as the way to keep it off for
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
