# Rendering shared by the diagnostic messages that the modules in this
# flake emit, so that one phrasing decision holds across every module
# class.
#
# The leading underscore in the filename excludes this file from
# "import-tree" in "../../flake.nix"; each module that wants these
# helpers imports it explicitly.
{
  # The phrase that opens a diagnostic addressed to one host, ready to
  # follow the word "Resolving": the quoted name when the host has
  # one, and a plain stand-in when it does not. The
  # "dotfiles.host.name" option is optional and purely
  # descriptive—nothing derives a value from it—so a host without a
  # name is ordinary, and its diagnostics read as prose rather than as
  # an empty pair of quotation marks.
  describeHost = name:
    if name == null
    then "an unnamed host"
    else ''host "${name}"'';

  # The sentence reporting one name written into both a selection list
  # and the matching exclusion list, whether a machine wrote the pair
  # or a user did.
  #
  # Pass the "forbidOption" argument when the machine also forbids the
  # name; only a machine ever passes it, since forbidding belongs to
  # the machine alone. Three lines then hold one name and two of them
  # keep it off independently, so the message states both and says
  # that acting on the selection means removing both rather than
  # either.
  describeContradiction = {
    hostLabel,
    selectionOption,
    exclusionOption,
    forbidOption ? null,
    name,
  }:
    if forbidOption == null
    then ''
      Resolving ${hostLabel}: "${name}" appears in both "${selectionOption}" and "${exclusionOption}". The exclusion prunes the name before the walk begins, so it holds and the selection has no effect. Remove whichever of the two lines does not say what you mean.
    ''
    else ''
      Resolving ${hostLabel}: "${name}" appears in "${selectionOption}", in "${exclusionOption}", and in "${forbidOption}". The exclusion and the forbid entry each keep the name inactive on their own, and forbidding binds every user besides, so removing either one alone leaves the selection just as ineffective. Remove both to act on the selection, or remove the selection.
    '';

  # The sentence reporting one name a machine both selects and
  # forbids. Forbidding admits no exception, so the selection can
  # never take effect anywhere, and neither line is redundant in the
  # way an exclusion beside a selection is: they state opposite
  # intents.
  describeForbiddenSelection = {
    hostLabel,
    selectionOption,
    forbidOption,
    name,
  }: ''
    Resolving ${hostLabel}: "${name}" appears in both "${selectionOption}" and "${forbidOption}". Forbidding prunes the name from every activation walk, the machine's own and every user's, and no user may undo it, so this selection can never take effect. Remove whichever of the two lines does not say what you mean.
  '';
}
