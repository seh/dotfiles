{lib, ...}: {
  options.flake = {
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
  };
}
