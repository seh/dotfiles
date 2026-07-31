{
  inputs,
  lib,
  ...
}: let
  hostName = "MyComputer";
  hostPlatform = "aarch64-darwin";
in {
  flake.darwinConfigurations = let
    identity = import ../../identity.nix;
    username = identity.name;
    darwinConfig = inputs.dotfiles.lib.mkDarwin {
      inherit hostPlatform;
      modules = [
        {
          # Set once at installation time; do not change.
          system.stateVersion = 7;

          dotfiles = {
            # This machine's own selections decide its own
            # configuration—every "nixDarwin" body follows these
            # alone—and are layered into every managed user's home, so
            # the machine provisions each user it manages. Select here
            # what every account on this machine should have: the
            # "essential" profile implies the features that configure
            # the machine itself, and "desktop" implies the Homebrew
            # casks.
            host = {
              name = hostName;
              profiles = [
                "desktop"
                "essential"
              ];
            };
            # When more than one user is defined under "users" below,
            # "primaryUser" must be set explicitly to one of those
            # usernames. With a single user (the common case), this
            # assignment can stay commented out; the dotfiles flake's
            # "modules/_darwin-primary-user.nix" file defaults the
            # "primaryUser" option to that sole user's name.
            # primaryUser = username;
            # A user's own selections add to the machine's, for that
            # user alone; no user's selection configures the machine.
            # List here what this person works with, beyond the
            # baseline above. See:
            # https://github.com/seh/dotfiles/tree/main/modules/home/profiles
            # https://github.com/seh/dotfiles/tree/main/modules/home/features
            users.${username} = {
              inherit identity;
              profiles = [
                "development"
              ];
              # Withhold a member of a selection made above,
              # for this user alone:
              # excludeProfiles = ["web"];
              features = [
                # "kubernetes"
                # "cloud/aws"
              ];
              interests = [
                # "lang/rust"
              ];
              homeManagerConfig = {
                home.stateVersion = "26.11";
              };
            };
          };
        }
      ];
    };
  in
    {
      ${hostName} = darwinConfig;
    }
    // lib.optionalAttrs (hostName != "local") {
      # By default nix-darwin will look for a configuration whose name
      # matches its hostname, per the value reported by invoking the
      # "scutil --get LocalHostName" command.
      #
      # We can use a general name here to establish the common case.
      local = darwinConfig;
    };
}
