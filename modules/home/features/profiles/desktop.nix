{
  config,
  lib,
  pkgs,
  ...
}: let
  hasTag = config.dotfiles._host.hasTag;
in {
  config = lib.mkMerge [
    {
      dotfiles._knownTags = ["desktop"];
    }
    (lib.mkIf (hasTag "desktop") {
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
