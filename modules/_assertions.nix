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
in {
  # Mismatch assertions run before unknown-name assertions so that
  # a misplaced name produces the more actionable diagnosis.
  assertions = map mismatchAssertion crossPairs ++ map unknownAssertion roles;
}
