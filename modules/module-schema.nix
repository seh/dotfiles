{lib, ...}: let
  # The "setOfNames" constructor builds the type for a set of names
  # written as a Nix list; its "merge" parameter selects how multiple
  # definitions of one option combine. See the constructor's comment
  # in the "modules/lib/_option-types.nix" file for the value form and
  # the accepted policies. Both registries below demand agreement:
  # differing sets are an error rather than a union.
  inherit (import ./lib/_option-types.nix {inherit lib;}) setOfNames preconditionSet;
in {
  options.dotfiles = {
    featureModules = lib.mkOption {
      # The "uniq" wrapper on the per-name leaf makes a second
      # registration of one feature name within the same class a
      # hard error rather than a silent merge of both bodies. The
      # same name may still register in two different classes (the
      # "nix" feature does, once for nix-darwin and once for NixOS).
      type = lib.types.lazyAttrsOf (lib.types.lazyAttrsOf (lib.types.uniq lib.types.deferredModule));
      default = {};
      description = ''
        Per-class feature modules, keyed by module class ("homeManager",
        "nixDarwin", "nixOS") and then by feature name. Each leaf value is a
        deferred module to be imported into that class's aggregate.
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
        profile, feature, or interest that brings the targets along.
        Each value is that source's "implies" list: an entry is
        either a bare target name or a record "{ name = "<target>";
        supportedPlatforms = [<systems>]; }" naming an edge present
        only when the host's platform is one of the listed systems.
        An interest source lists only bare interest names. Populated
        by the "mkFeature", "mkProfile", and "mkInterest" functions
        from their "implies" argument. Definitions accumulate, so
        several modules may extend one source's edges. The
        "implicationsFor" function in "modules/lib/_implications.nix"
        assembles these into the role-keyed implication graph.
        Populates "dotfiles._impliedEdges" in each class aggregator.
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
        Feature names advertised by feature or profile modules in this
        flake or downstream consumers. Accumulated and de-duplicated.
        Populates "dotfiles._knownFeatures" in each class aggregator.
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
    knownProfiles = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      apply = lib.unique;
      description = ''
        Profile names advertised by profile modules in this flake or
        downstream consumers. Accumulated and de-duplicated. Populates
        "dotfiles._knownProfiles" in each class aggregator.
      '';
    };
    profileModules = lib.mkOption {
      # See the note on "featureModules" above for the "uniq"
      # wrapper's purpose.
      type = lib.types.lazyAttrsOf (lib.types.lazyAttrsOf (lib.types.uniq lib.types.deferredModule));
      default = {};
      description = ''
        Per-class profile modules, keyed by module class ("homeManager",
        "nixDarwin", "nixOS") and then by profile name. Each leaf value is a
        deferred module to be imported into that class's aggregate.

        Profiles, features, and interests share one namespace: a
        profile may not share its name with a feature or an interest.
        An assertion in "modules/_assertions.nix" rejects collisions.
      '';
    };
    profileSupportedPlatforms = lib.mkOption {
      type = lib.types.attrsOf (setOfNames {
        merge = "agreement";
        allowEmpty = false;
      });
      default = {};
      description = ''
        Per-profile platform support, keyed by profile name. Each value
        lists the Nixpkgs system identifiers on which that profile may
        activate; a host qualifies when its platform is one of them.
        Profiles absent from this registry may activate on every
        platform. Populated by the "mkProfile" function from its
        "supportedPlatforms" argument. The value is a set of names:
        the type's merge normalizes each definition, so declarations
        denoting the same set merge and ones denoting different sets
        are rejected. Populates
        "dotfiles._profileSupportedPlatforms" in each class
        aggregator.
      '';
    };
  };
}
