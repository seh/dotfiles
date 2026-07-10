{lib, ...}: {
  options.dotfiles = {
    featureModules = lib.mkOption {
      type = lib.types.lazyAttrsOf (lib.types.lazyAttrsOf lib.types.deferredModule);
      default = {};
      description = ''
        Per-class feature modules, keyed by module class ("homeManager",
        "nixDarwin", "nixOS") and then by feature name. Each leaf value is a
        deferred module to be imported into that class's aggregate.
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
      type = lib.types.lazyAttrsOf (lib.types.lazyAttrsOf lib.types.deferredModule);
      default = {};
      description = ''
        Per-class profile modules, keyed by module class ("homeManager",
        "nixDarwin", "nixOS") and then by profile name. Each leaf value is a
        deferred module to be imported into that class's aggregate.

        Profiles live in a namespace distinct from features so that a
        profile and a feature may share the same name.
      '';
    };
    profileSupportedPlatforms = lib.mkOption {
      type = lib.types.attrsOf (
        lib.mkOptionType {
          name = "nonEmptyEqualListOfStr";
          description = "non-empty list of strings, equal across all definitions";
          # NB: The empty list is rejected deliberately: it would
          # mean "supported nowhere", which is better expressed by
          # not registering the profile at all, and it is too easy
          # to write while meaning "no constraint" (which omitting
          # the attribute expresses).
          check = v: builtins.isList v && v != [] && lib.all builtins.isString v;
          merge = lib.options.mergeEqualOption;
        }
      );
      default = {};
      description = ''
        Per-profile platform support, keyed by profile name. Each value
        lists the Nixpkgs system identifiers on which that profile may
        activate; a host qualifies when its platform is one of them.
        Profiles absent from this registry may activate on every
        platform. Populated by the "mkProfile" function from its
        "supportedPlatforms" argument (normalized to a sorted, unique
        list so that equal declarations merge and unequal ones are
        rejected). Populates "dotfiles._profileSupportedPlatforms" in
        each class aggregator.
      '';
    };
  };
}
