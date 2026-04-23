{
  dotfiles.knownProfiles = ["macos"];

  dotfiles.profileModules.homeManager.macos = {
    config,
    lib,
    pkgs,
    ...
  }: let
    inherit (lib) mkDefault mkIf;
    inherit (config.dotfiles._host) activatesProfile;
  in {
    config = mkIf (activatesProfile "macos") {
      home = {
        packages = with pkgs; [
        ];
      };

      targets.darwin = {
        # TODO(SEHarris): Remove these once all machines are using Home
        # Manager's state version 25.11 or newer.
        copyApps.enable = mkDefault true;
        linkApps.enable = mkDefault false;

        defaults = {
          "com.apple.dock" = {
            # System Preferences > Mission Control > Group windows by application
            expose-group-apps = mkDefault true;
            # System Preferences > Dock > Position on screen
            orientation = mkDefault "right";
            # System Preferences > Dock > Show recent applications in Dock
            show-recents = mkDefault false;
            #size-immutable
            # System Preferences > Dock > Size
            tilesize = mkDefault 39;
          };
        };
      };
    };
  };
}
