{
  # The "all" tag is an aggregate: its expansion into concrete leaf
  # tags is defined centrally by "cascadesFor" in modules/lib and
  # applied before the module system sees the host's tag list. The
  # advertisement below exists solely so that
  # "modules/_assertions.nix" accepts "all" in a host's declared tag
  # list.
  flake.knownTags = ["all"];

  flake.profileModules.homeManager.all = {...}: {};
}
