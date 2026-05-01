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
  # apply to this host. The "all" profile is an umbrella that
  # expands to every profile this flake advertises, computed from
  # the "knownProfiles" registry. To opt out of specific umbrella
  # members (such as the Firefox/Safari customization in "web"),
  # list them under "excludeProfiles". Add specific features such
  # as "kubernetes", "cloud/aws", or "lang/rust" under "features".
  # See:
  # https://github.com/seh/dotfiles/tree/main/modules/home/profiles
  # https://github.com/seh/dotfiles/tree/main/modules/home/features
  hostRecord = {
    name = hostName;
    framework = "nixDarwin";
    platform = hostPlatform;
    profiles = [
      "all"
    ];
    # Opt out of specific umbrella members. For example, to skip
    # Firefox/Safari customization:
    # excludeProfiles = ["web"];
    features = [
      # "kubernetes"
      # "cloud/aws"
      # "lang/rust"
    ];
  };

  homeManagerConfig = {
    home.stateVersion = "25.11";
  };
in {
  flake.darwinConfigurations = let
    darwinConfig = inputs.dotfiles.lib.mkDarwin {
      inherit hostPlatform;
      pkgs = inputs.dotfiles.lib.pkgsFor hostPlatform;
      modules = [
        {dotfiles.host = hostRecord;}
        identity
        {
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
}
