# Assertion that every tag in a host's declared "tags" list appears
# in "dotfiles._knownTags" (the union of tags advertised by imported
# feature modules). This catches typos in host records without
# requiring each feature to reject unknown tags individually.
{
  lib,
  config,
  ...
}: {
  assertions = let
    hostTags = config.dotfiles._host.tags;
    known = config.dotfiles._knownTags;
    unknown = lib.subtractLists known hostTags;
  in [
    {
      assertion = unknown == [];
      message = ''
        Host "${toString config.dotfiles._host.name}" declares tag(s) that no imported feature module advertises: ${lib.concatStringsSep ", " unknown}.
        Either add the corresponding feature or remove the tag(s) from the host's "tags" list.
      '';
    }
  ];
}
