{
  # The "all" profile is an umbrella that expands to every profile
  # this flake advertises. Its targets are computed dynamically by
  # "cascadesFor" in modules/lib from the "knownProfiles" registry,
  # so introducing a new profile folds it into "all" automatically.
  # Hosts opt out of specific umbrella members via
  # "excludeProfiles". The advertisement below exists solely so
  # that "modules/_assertions.nix" accepts "all" in a host's
  # declared "profiles" list.
  dotfiles.knownProfiles = ["all"];
}
