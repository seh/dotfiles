# Rendering shared by the diagnostic messages that the modules in this
# flake emit, so that one phrasing decision holds across every module
# class.
#
# The leading underscore in the filename excludes this file from
# "import-tree" in "../../flake.nix"; each module that wants these
# helpers imports it explicitly.
{
  # The phrase that opens a diagnostic addressed to one host, ready to
  # follow the word "Resolving": the quoted name when the host carries
  # one, and a plain stand-in when it does not. The
  # "dotfiles.host.name" option is optional and purely
  # descriptive—nothing derives a value from it—so a host without a
  # name is ordinary, and its diagnostics read as prose rather than as
  # an empty pair of quotation marks.
  describeHost = name:
    if name == null
    then "an unnamed host"
    else ''host "${name}"'';
}
