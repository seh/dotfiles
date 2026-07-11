{
  # The "all" profile expands to every other profile this flake
  # advertises. Its targets are computed dynamically by the
  # "implicationsFor" function in "modules/lib/_implications.nix"
  # from the "knownProfiles" registry, so introducing a new profile
  # folds it into "all" automatically. Hosts opt out of specific
  # members via "excludeProfiles". The advertisement below exists
  # solely so that "modules/_assertions.nix" accepts "all" in a host's
  # selected "profiles" list.
  dotfiles.knownProfiles = ["all"];
}
