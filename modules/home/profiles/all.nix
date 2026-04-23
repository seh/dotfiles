{
  flake.profileModules.homeManager.all = {...}: {
    # The "all" tag is an aggregate: its expansion into concrete leaf
    # tags is defined centrally by "cascadesFor" in modules/lib and
    # applied before the module system sees the host's tag list. This
    # file exists solely to advertise "all" as a known tag so that
    # modules/_assertions.nix accepts it in a host's declared tag
    # list.
    config.dotfiles._knownTags = ["all"];
  };
}
