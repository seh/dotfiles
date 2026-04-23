{
  # The "all" profile is an aggregate: its expansion into concrete
  # leaf profiles is defined centrally by "cascadesFor" in
  # modules/lib and applied inside the host submodule before the
  # module system sees the host's resolved profile list. The
  # advertisement below exists solely so that
  # "modules/_assertions.nix" accepts "all" in a host's declared
  # "profiles" list.
  flake.knownProfiles = ["all"];

  flake.profileModules.homeManager.all = {...}: {};
}
