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
            host = {
              name = hostName;
            };
            # When more than one user is defined under "users" below,
            # "primaryUser" must be set explicitly to one of those
            # usernames. With a single user (the common case), this
            # assignment can stay commented out; the dotfiles flake's
            # "modules/_darwin-primary-user.nix" file defaults the
            # "primaryUser" option to that sole user's name.
            # primaryUser = username;
            # Activate feature modules by listing profiles and
            # features that apply to this user. The "all" profile
            # expands to every other profile this flake advertises,
            # computed from the "knownProfiles" registry. To opt out
            # of specific members of the "all" profile (such as the
            # Firefox/Safari customization in "web"), list them under
            # "excludeProfiles". Add specific features such as
            # "kubernetes", "cloud/aws", or "lang/rust" under
            # "features". See:
            # https://github.com/seh/dotfiles/tree/main/modules/home/profiles
            # https://github.com/seh/dotfiles/tree/main/modules/home/features
            users.${username} = {
              inherit identity;
              profiles = [
                "all"
              ];
              # Opt out of specific members of the "all" profile. For
              # example, to skip Firefox/Safari customization:
              # excludeProfiles = ["web"];
              features = [
                # "kubernetes"
                # "cloud/aws"
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
