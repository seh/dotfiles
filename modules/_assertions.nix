# Assertions catching mis-declared host entries.
#
# The original flat assertion (every tag in "dotfiles._host.tags"
# appears in "dotfiles._knownTags") stays in place unchanged.
#
# Four new role-aware assertions are added for the typed activation
# surface. They are gated on non-empty registries so they stay
# silent until profile/feature modules begin advertising under
# "flake.knownProfiles" / "flake.knownFeatures".
#
# Role-mismatch assertions fire before the plain "unknown name"
# assertions so that the more specific diagnosis wins when a name
# was simply declared under the wrong role.
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
      known = config.dotfiles._knownProfiles;
      humanSingular = "profile";
      humanPlural = "profile(s)";
      seedOption = "dotfiles._host.profiles";
    }
    {
      name = "features";
      declared = host.features;
      known = config.dotfiles._knownFeatures;
      humanSingular = "feature";
      humanPlural = "feature(s)";
      seedOption = "dotfiles._host.features";
    }
  ];

  # Pair each role with "the other roles", so that each role's
  # declarations can be checked against every other role's known
  # set. For the two-role case this degenerates to one pair per
  # direction, which matches the four assertion shapes the
  # migration plan specifies.
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
    assertion = there.known == [] || misplaced == [];
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
    assertion = role.known == [] || unknown == [];
    message = ''
      Resolving host "${hostName}": declaring ${role.humanPlural} that no imported ${role.humanSingular} module advertises: ${lib.concatStringsSep ", " unknown}. Add the corresponding ${role.humanSingular} module or remove the name(s) from "${role.seedOption}".
    '';
  };

  legacyTagsAssertion = let
    hostTags = host.tags;
    known = config.dotfiles._knownTags;
    unknown = lib.subtractLists known hostTags;
  in {
    assertion = unknown == [];
    message = ''
      Host "${hostName}" declares tag(s) that no imported feature module advertises: ${lib.concatStringsSep ", " unknown}.
      Either add the corresponding feature or remove the tag(s) from the host's "tags" list.
    '';
  };
in {
  # Mismatch assertions run before unknown-name assertions so that
  # a misplaced name produces the more actionable diagnosis.
  assertions =
    [
      legacyTagsAssertion
    ]
    ++ map mismatchAssertion crossPairs
    ++ map unknownAssertion roles;
}
