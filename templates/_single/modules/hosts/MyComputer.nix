{
  inputs,
  lib,
  ...
}: let
  hostName = "MyComputer";
  # Likely alternative: "x86_64-darwin"
  hostPlatform = "aarch64-darwin";

  identity = import ../../identity.nix;
  username = identity.dotfiles.user.name;

  # Activate feature modules by listing profiles and features that
  # apply to this host. The "all" profile is a convenience aggregate
  # that expands (via the cascade table in the dotfiles flake) to
  # "essential", "development", "desktop", "fonts", and (on Darwin)
  # "macos". Add specific features such as "kubernetes", "aws", or
  # "rust" under "features". See:
  # https://github.com/seh/dotfiles/tree/main/modules/home/profiles
  # https://github.com/seh/dotfiles/tree/main/modules/home/features
  hostRecord = {
    name = hostName;
    framework = "nixDarwin";
    platform = hostPlatform;
    profiles = [
      "all"
    ];
    features = [
      # "kubernetes"
      # "aws"
      # "rust"
    ];
  };

  homeManagerConfig = {
    home.stateVersion = "25.11";
  };
in {
  flake = {
    homeConfigurations = {
      "${username}@${hostName}.local" = inputs.dotfiles.lib.mkHome {
        pkgs = inputs.dotfiles.lib.pkgsFor hostPlatform;
        host = hostRecord;
        modules = [
          identity
          homeManagerConfig
          {programs.home-manager.enable = true;}
        ];
      };
    };

    darwinConfigurations = let
      darwinConfig = inputs.dotfiles.lib.mkDarwin {
        inherit hostPlatform;
        pkgs = inputs.dotfiles.lib.pkgsFor hostPlatform;
        host = hostRecord;
        modules = [
          identity
          {
            # nix-darwin profiles are advertised under:
            # https://github.com/seh/dotfiles/tree/main/modules/nix-darwin/profiles
            dotfiles._host.profiles = ["apps"];

            # Override some default values as necessary:
            # system.stateVersion = 4; # Default is 6

            home-manager.users.${username} = homeManagerConfig;
          }
        ];
      };
    in
      {
        ${hostName} = darwinConfig;
      }
      // lib.optionalAttrs (hostName != "local") {
        # By default nix-darwin will look for a configuration
        # whose name matches its hostname, per the value
        # reported by invoking the "scutil --get
        # LocalHostName" command.
        #
        # We can use a general name here to establish the
        # common case.
        local = darwinConfig;
      };
  };
}
