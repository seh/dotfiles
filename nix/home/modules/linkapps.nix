# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/1c190d0ac1d87c159b8b7d777f02261ae58a3fc5/nix/home/modules/linkapps.nix
{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf hm;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;

  destination = "${config.home.homeDirectory}/Applications/Home Manager";

  apps = pkgs.buildEnv {
    name = "home-manager-applications";
    paths = config.home.packages;
    pathsToLink = "/Applications";
  };
in
{
  # Disable Home Manager's built-in application linking. The built-in
  # one uses symbolic links, which Spotlight does not index.
  #
  # Replace creation of this "~/Applications/Home Manager Apps"
  # directory, per the following:
  # https://github.com/nix-community/home-manager/blob/b23c7501f7e0a001486c9a5555a6c53ac7b08e85/modules/targets/darwin/linkapps.nix#L6-L12
  disabledModules = [ "targets/darwin/linkapps.nix" ];

  config = mkIf isDarwin {
    home.activation.linkMacOSApplications = hm.dag.entryAfter [ "writeBoundary" ] ''
      linkMacOSApplications() {
        $VERBOSE_ECHO 'Creating aliases for macOS Applications'
        $DRY_RUN_CMD rm -rf $VERBOSE_ARG '${destination}'
        $DRY_RUN_CMD mkdir -p $VERBOSE_ARG '${destination}'

        local app app_path
        find -L "${apps}/Applications" -mindepth 1 -maxdepth 1 -type d -print0 \
            | while IFS= read -rd "" app; do
          if ! app_path="$(realpath -e "$app")"; then
            continue
          fi

          $DRY_RUN_CMD /usr/bin/osascript \
            -e 'tell app "Finder"' \
            -e "make new alias file at POSIX file \"${destination}\" to POSIX file \"$app_path\"" \
            -e "set name of result to \"''${app_path##*/}\"" \
            -e 'end tell'
        done
      }
      linkMacOSApplications
    '';
  };
}
