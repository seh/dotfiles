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
        names the classes ("homeManager", "nixDarwin", "nixOS") that
        register a body for that feature, in that order. This module
        computes it from the "featureModules" registry; the
        "mkFeature" function records nothing here. The record
        therefore stays faithful to the bodies actually present: a
        feature registered by name alone, with no bodies, is absent. A
        feature may configure the machine alone—the "apps" feature
        carries a nix-darwin body and nothing else—so this record also
        diagnoses a user selecting such a feature. The keys alone tell
        which features configure something, which is how the
        "implicationsFor" function keeps the computed "all" feature on
        the body-less bundles. Each class aggregator mirrors it into
        "dotfiles._featureClasses".
      '';
    };
    featureModules = lib.mkOption {
      # The "uniq" wrapper on the per-name leaf makes a second
      # registration of one feature name within the same class a
      # hard error rather than a silent merge of both bodies. The
      # same name may still register in two different classes (the
      # "nix" feature does, once for nix-darwin and once for NixOS).
      type = lib.types.lazyAttrsOf (lib.types.lazyAttrsOf (lib.types.uniq lib.types.deferredModule));
      default = {};
      description = ''
        Per-class feature modules, keyed by module class
        ("homeManager", "nixDarwin", "nixOS") and then by feature
        name. Each leaf value is a deferred module to be imported into
        that class's aggregate.

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
        keyed contingent feature activates; listing a name here
        never activates it. An entry is either a bare feature or
        interest name (satisfied when that name is active) or a group
        "{ anyOf = [ "<name>" ... ]; }" (satisfied when at least one
        member is active). A feature present in this registry is a
        "contingent feature": it activates automatically exactly when
        every one of its preconditions is satisfied, and only then.
        Populated by the "mkFeature" function from its "preconditions"
        argument. The value is a set of preconditions: the type's
        merge normalizes each definition, so definitions that spell
        the same set merge, and the merge rejects ones that differ.
        Populates "dotfiles._featurePreconditions" in each class
        aggregator.
      '';
    };
    impliedEdges = lib.mkOption {
      type = lib.types.attrsOf (lib.types.listOf lib.types.raw);
      default = {};
      description = ''
        Per-source implied edges, keyed by the name of the source
        feature or interest that brings the targets along. Each value
        is that source's "implies" list: an entry is either a bare
        target name or a record "{ name = "<target>";
        supportedPlatforms = [<systems>]; }" naming an edge present
        only when the host's platform is one of the listed systems. An
        interest source lists only bare interest names. Populated by
        the "mkFeature" and "mkInterest" functions from their
        "implies" argument. Definitions accumulate, so several modules
        may extend one source's edges. The "implicationsFor" function
        in "modules/lib/_implications.nix" assembles these into the
        implication graph. Populates "dotfiles._impliedEdges" in each
        class aggregator.
      '';
    };
    interestDescriptions = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = {};
      description = ''
        Per-interest prose descriptions, keyed by interest name.
        Populated by the "mkInterest" function from its optional
        "description" field. Nothing consumes this registry yet: it
        waits for a diagnostic or documentation surface that wants
        the prose, and until one arrives the field is stored here
        and nowhere else.
      '';
    };
    knownFeatures = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      apply = lib.unique;
      description = ''
        Feature names advertised by feature modules in this flake or
        downstream consumers. Accumulated and de-duplicated. Populates
        "dotfiles._knownFeatures" in each class aggregator.
      '';
    };
    knownInterests = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      apply = lib.unique;
      description = ''
        Interest names advertised via the "mkInterest" function in
        this flake or downstream consumers. An interest is a named
        want that participates in activation exactly as a feature
        does—a host may select or exclude it, and a contingent feature
        may name it as a precondition—but carries no configuration.
        Accumulated and de-duplicated. Populates
        "dotfiles._knownInterests" in each class aggregator.
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
        platform. Populated by the "mkFeature" function from its
        "supportedPlatforms" argument. The value is a set of names:
        the type's merge normalizes each definition, so definitions
        that spell the same set merge, and the merge rejects ones
        that differ. Populates "dotfiles._supportedPlatforms" in each
        class aggregator.
      '';
    };
  };

  config.dotfiles.featureClasses = classesOf config.dotfiles.featureModules;
}
