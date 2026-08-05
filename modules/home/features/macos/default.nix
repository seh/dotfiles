{
  flakeLib,
  lib,
  ...
}:
flakeLib.mkFeature "macos" {
  # This feature configures macOS itself, so a host qualifies for it
  # only when its platform is one of the Darwin systems.
  supportedPlatforms = lib.platforms.darwin;

  homeManager = {lib, ...}: let
    inherit (lib) mkDefault;
  in {
    home = {
      packages = [
      ];
    };

    targets.darwin = {
      # TODO(seh): Remove these once all machines are using Home
      # Manager's state version 25.11 or newer.
      copyApps.enable = mkDefault true;
      linkApps.enable = mkDefault false;

      defaults = {
        "com.apple.dock" = {
          # "System Settings"
          #   > Desktop & Dock
          #     > Mission Control
          #       > Group windows by application
          expose-group-apps = mkDefault true;
          # "System Settings"
          #   > Desktop & Dock
          #     > Dock position on screen
          orientation = mkDefault "right";
          # "System Settings"
          #   > Desktop & Dock
          #     > Show suggested and recent apps in Dock
          show-recents = mkDefault false;
          #size-immutable
          # "System Settings"
          #   > Desktop & Dock
          #     > Size
          tilesize = mkDefault 39;
        };
      };
    };
  };
}
