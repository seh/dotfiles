{
  dotfiles.knownProfiles = ["desktop"];

  dotfiles.profileModules.homeManager.desktop = {
    config,
    lib,
    pkgs,
    ...
  }: let
    inherit (config.dotfiles._host) activatesProfile;
  in {
    config = lib.mkIf (activatesProfile "desktop") {
      home.packages = let
        candidatePkg = pkgs.zoom-us;
      in
        lib.optionals (lib.meta.availableOn pkgs.stdenv.hostPlatform candidatePkg) [
          candidatePkg
        ];
    };
  };
}
