{
  config,
  lib,
  ...
}: let
  hasTag = config.dotfiles._resolved.hasTag;
in {
  config = lib.mkMerge [
    {
      dotfiles._knownTags = ["all"];
    }
    (lib.mkIf (hasTag "all") {
      # The "all" tag expands into the three aggregate tags. Each of
      # those in turn may expand into further tags (e.g. "desktop"
      # implies "fonts" and "macos") through their own module files.
      dotfiles._resolved.tags = [
        "essential"
        "development"
        "desktop"
      ];
    })
  ];
}
