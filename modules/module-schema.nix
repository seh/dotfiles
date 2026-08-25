{
  config,
  lib,
  ...
}: let
  # The "setOfNames" constructor builds the type for a set of names
  # written as a Nix list; its "merge" parameter selects how multiple
  # definitions of one option combine. See the constructor's comment
  # in the "modules/lib/_option-types.nix" file for the value form and
  # the accepted policies. Both registries below demand agreement:
  # differing sets are an error rather than a union.
  inherit (import ./lib/_option-types.nix {inherit lib;}) setOfNames preconditionSet;

  # The classes that register a body for each name in a per-class
  # module registry, keyed by name. The derivation reads the
  # registry's keys alone and leaves every body unforced.
  classesOf = registry:
    lib.zipAttrsWith (_name: classes: classes) (
      lib.mapAttrsToList (
        class: bodies: lib.mapAttrs (_name: _body: class) bodies
      )
      registry
    );
in {
  options.dotfiles = {
    featureClasses = lib.mkOption {
      type = lib.types.attrsOf (lib.types.listOf lib.types.str);
      readOnly = true;
      description = ''
        Per-feature module classes, keyed by feature name. Each value
        lists the classes ("homeManager", "nixDarwin", "nixOS") that
        register a body for that feature, in that order. This module
        computes it from the "featureModules" registry; the
        "mkFeature" function records nothing here. The record
        therefore stays faithful to the bodies actually present: a
        feature registered by name alone, with no bodies, is absent. A
        feature may configure the machine alone—the "apps" feature
        registers a nix-darwin body and nothing else—so this record
        also diagnoses a user selecting such a feature. The keys alone
        tell which features configure something, which is how the
        "implicationsFor" function keeps the computed "all" feature on
        the body-less bundles. Each class aggregator mirrors it into
        the "dotfiles._featureClasses" option.
      '';
    };
    featureModules = lib.mkOption {
      # The "uniq" wrapper on the per-name leaf makes a second
      # registration of one feature name within the same class a hard
      # error rather than a silent merge of both bodies. The same name
      # may still register in two different classes (the "nix" feature
      # does, once for nix-darwin and once for NixOS).
      type = lib.types.lazyAttrsOf (lib.types.lazyAttrsOf (lib.types.uniq lib.types.deferredModule));
      default = {};
      description = ''
        Per-class feature modules, keyed by module class
        ("homeManager", "nixDarwin", "nixOS") and then by feature
        name. Each leaf value is a deferred module that the class's
        aggregate imports.

        Features and interests share one namespace: a feature may not
        share its name with an interest. An assertion in the
        "modules/_assertions.nix" file rejects collisions.
      '';
    };
    featurePreconditions = lib.mkOption {
      type = lib.types.attrsOf (preconditionSet {});
      default = {};
      description = ''
        Per-feature preconditions, keyed by feature name. Each value
        is a conjunction of entries that must all be satisfied before
        the keyed contingent feature activates; listing a name here
        never activates it. An entry is either a bare feature or
        interest name (satisfied when that name is active) or a group
        "{ anyOf = [ "<name>" ... ]; }" (satisfied when at least one
        member is active). A feature present in this registry is a
        "contingent feature": it activates automatically exactly when
        every one of its preconditions is satisfied, and only then.
        The "mkFeature" function fills it from its "preconditions"
        argument. The value is a set of preconditions: the type's
        merge normalizes each definition, so definitions that spell
        the same set merge, and the merge rejects ones that differ.
        Each class aggregator mirrors it into the
        "dotfiles._featurePreconditions" option.
      '';
    };
    impliedEdges = lib.mkOption {
      type = lib.types.attrsOf (lib.types.listOf lib.types.raw);
      default = {};
      description = ''
        Per-source implied edges, keyed by the name of the source
        feature or interest that brings the targets along. Each value
        is that source's "implies" list. An entry is either a bare
        target name or a record "{ name = "<target>";
        supportedPlatforms = [<systems>]; }" for an edge present only
        when the host's platform is one of the listed systems. An
        interest source lists only bare interest names. The
        "mkFeature" and "mkInterest" functions fill it from their
        "implies" argument. Definitions accumulate, so several modules
        may extend one source's edges. The "implicationsFor" function
        in the "modules/lib/_implications.nix" file assembles these
        into the implication graph. Each class aggregator mirrors it
        into the "dotfiles._impliedEdges" option.
      '';
    };
    interestDescriptions = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = {};
      description = ''
        Per-interest prose descriptions, keyed by interest name. The
        "mkInterest" function fills it from its optional "description"
        argument. Nothing consumes this registry yet: it waits for a
        diagnostic or documentation surface that wants the prose, and
        until one arrives this registry alone contains it.
      '';
    };
    knownFeatures = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      apply = lib.unique;
      description = ''
        The names of every feature a feature module advertises, in
        this flake or a downstream consumer. Definitions accumulate
        through the "listOf" type's append-merge; the "apply" function
        drops duplicates. Each class aggregator mirrors it into the
        "dotfiles._knownFeatures" option.
      '';
    };
    knownInterests = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      apply = lib.unique;
      description = ''
        The names of every interest a module declares via the
        "mkInterest" function, in this flake or a downstream consumer.
        An interest is a named want that participates in activation
        exactly as a feature does—a host may select or exclude it, and
        a contingent feature may list it as a precondition—but
        declares no configuration. Definitions accumulate through the
        "listOf" type's append-merge; the "apply" function drops
        duplicates. Each class aggregator mirrors it into the
        "dotfiles._knownInterests" option.
      '';
    };
    supportedPlatforms = lib.mkOption {
      type = lib.types.attrsOf (setOfNames {
        merge = "agreement";
        allowEmpty = false;
      });
      default = {};
      description = ''
        Per-name platform support, keyed by feature name. Each value
        lists the Nixpkgs system identifiers on which that name may
        activate; a host qualifies when its platform is one of them.
        Names absent from this registry may activate on every
        platform. The "mkFeature" function fills it from its
        "supportedPlatforms" argument. The value is a set of names:
        the type's merge normalizes each definition, so definitions
        that spell the same set merge, and the merge rejects ones that
        differ. Each class aggregator mirrors it into the
        "dotfiles._supportedPlatforms" option.
      '';
    };
    unfreePackages = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      apply = lib.unique;
      description = ''
        The unfree packages that feature modules in this flake or
        downstream consumers install, each spelled as the string that
        the "lib.getName" function yields for the package. The
        "mkFeature" function fills it from its "unfreePackages"
        argument. Definitions accumulate and the "apply" function
        drops duplicates, so every feature declares its own beside the
        packages it installs rather than in one central list. The
        "modules/nixpkgs-config.nix" module publishes the union as the
        "flake.allowUnfreePackages" output, which both nixpkgs
        instantiation sites hand to nixpkgs' own "allowUnfreePackages"
        option: the flake-parts evaluator's "perSystem" package set
        and the "pkgsFor" function in the
        "modules/lib/_constructors.nix" file. One flat set serves
        every instantiation, whatever platform it targets and whatever
        features the host activates, since tolerating a package that
        nothing installs costs nothing while installing one without
        toleration halts evaluation.
      '';
    };
  };

  config.dotfiles.featureClasses = classesOf config.dotfiles.featureModules;
}
