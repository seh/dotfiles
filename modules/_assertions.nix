# Assertions catching mis-declared host entries.
#
# Role-aware assertions are surfaced against the typed activation
# surface: each role ("profiles", "features") gets an unknown-name
# assertion, plus a cross-role mismatch assertion that catches a
# name declared under one role but advertised under another.
#
# Role-mismatch assertions fire before the plain "unknown name"
# assertions so that the more specific diagnosis wins when a name
# was simply declared under the wrong role.
#
# The "knownProfiles" and "knownFeatures" registries consulted here
# are flake-wide by design; see the comment block in
# "modules/_tags.nix" near the declaration of
# "dotfiles._knownProfiles" for the rationale.
{
  lib,
  config,
  ...
}: let
  host = config.dotfiles._host;
  hostName = toString host.name;

  # Role-parametric driver. Iterating over this list (rather than
  # hard-coding "profiles" and "features") means that adding a third
  # role later — say, "bundles" — reduces to a single new entry.
  roles = [
    {
      name = "profiles";
      declared = host.profiles;
      excluded = host.excludeProfiles;
      known = config.dotfiles._knownProfiles;
      humanSingular = "profile";
      humanPlural = "profile(s)";
      seedOption = "dotfiles._host.profiles";
      excludeOption = "dotfiles._host.excludeProfiles";
    }
    {
      name = "features";
      declared = host.features;
      excluded = host.excludeFeatures;
      known = config.dotfiles._knownFeatures;
      humanSingular = "feature";
      humanPlural = "feature(s)";
      seedOption = "dotfiles._host.features";
      excludeOption = "dotfiles._host.excludeFeatures";
    }
  ];

  # Pair each role with "the other roles", so that each role's
  # declarations can be checked against every other role's known
  # set. For the two-role case this degenerates to one pair per
  # direction.
  crossPairs =
    lib.concatMap (
      here: map (there: {inherit here there;}) (builtins.filter (r: r.name != here.name) roles)
    )
    roles;

  # Role-mismatch assertion: names declared under "here.name" that
  # are actually known as "there.name".
  mismatchAssertion = {
    here,
    there,
  }: let
    misplaced = builtins.filter (n: builtins.elem n there.known) here.declared;
  in {
    assertion = misplaced == [];
    message = ''
      Resolving host "${hostName}": declaring ${lib.concatStringsSep ", " misplaced} under "${here.name}", but these are known ${there.humanPlural}. Move them to "${there.seedOption}".
    '';
  };

  # Unknown-name assertion for a single role. Elements that are
  # neither in "role.known" nor in any other role's "known" set
  # count as unknown; elements present in another role's "known"
  # set are instead caught by the role-mismatch assertion above.
  unknownAssertion = role: let
    otherKnown = lib.concatLists (map (r: r.known) (builtins.filter (r: r.name != role.name) roles));
    unknown =
      builtins.filter (
        n: !(builtins.elem n role.known) && !(builtins.elem n otherKnown)
      )
      role.declared;
  in {
    assertion = unknown == [];
    message = ''
      Resolving host "${hostName}": declaring ${role.humanPlural} that no imported ${role.humanSingular} module advertises: ${lib.concatStringsSep ", " unknown}. Add the corresponding ${role.humanSingular} module or remove the name(s) from "${role.seedOption}".
    '';
  };

  # Unknown-exclude assertion: any name listed under a role's
  # "exclude" option must also appear in that role's known set.
  # A misspelled exclusion would otherwise silently fail to
  # suppress, leaving the host with an unintended activation set.
  unknownExcludeAssertion = role: let
    unknown = builtins.filter (n: !(builtins.elem n role.known)) role.excluded;
  in {
    assertion = unknown == [];
    message = ''
      Resolving host "${hostName}": excluding ${role.humanPlural} that no imported ${role.humanSingular} module advertises: ${lib.concatStringsSep ", " unknown}. Correct the spelling or remove the name(s) from "${role.excludeOption}".
    '';
  };

  # Soft check: known feature names should follow the scoped naming
  # convention "[scope/]name", with one or more "/"-separated segments
  # of lowercase letters, digits, and hyphens. Each segment must start
  # with a lowercase letter.
  scopedNamePattern = "[a-z][a-z0-9-]*(/[a-z][a-z0-9-]*)*";
  badFeatureNames =
    lib.filter (
      n: builtins.match scopedNamePattern n == null
    )
    config.dotfiles._knownFeatures;
in {
  # Mismatch assertions run before unknown-name assertions so that
  # a misplaced name produces the more actionable diagnosis.
  assertions =
    map mismatchAssertion crossPairs ++ map unknownAssertion roles ++ map unknownExcludeAssertion roles;

  warnings = lib.optional (badFeatureNames != []) ''
    These known feature names do not follow the scoped naming
    convention "[scope/]name" with lowercase letters, digits, and
    hyphens (one or more "/"-separated segments):
    ${lib.concatMapStringsSep "\n" (n: "  - ${n}") badFeatureNames}
  '';
}
