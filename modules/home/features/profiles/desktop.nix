{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  hasTag = config.dotfiles._host.hasTag;
in {
  config = lib.mkMerge [
    {
      dotfiles._knownTags = ["desktop"];
    }
    (lib.mkIf (hasTag "desktop") {
      # The "desktop" tag expands into "fonts" always and "macos"
      # on Darwin hosts, honoring the original cascade semantics.
      dotfiles._host.tags = ["fonts"] ++ lib.optional isDarwin "macos";

      home.packages = let
        candidatePkg = pkgs.zoom-us;
      in
        lib.optionals (lib.meta.availableOn pkgs.stdenv.hostPlatform candidatePkg) [
          candidatePkg
        ];

      dotfiles.emacs.enable = lib.mkDefault true;
    })
  ];
}
