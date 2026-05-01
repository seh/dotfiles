{inputs, ...}: let
  hostName = "MyComputer";
  # Likely alternatives: "aarch64-darwin", "x86_64-darwin",
  # "x86_64-linux".
  hostPlatform = "aarch64-linux";

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
    framework = "homeManager";
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
    # Set once at installation time; do not change.
    home.stateVersion = "25.11";

    # Add further Home Manager modules or option assignments here,
    # such as "programs.git.enable = true;" or imports of locally
    # defined modules.
  };
in {
  flake.homeConfigurations = {
    "${username}@${hostName}" = inputs.dotfiles.lib.mkHome {
      pkgs = inputs.dotfiles.lib.pkgsFor hostPlatform;
      modules = [
        {dotfiles.host = hostRecord;}
        identity
        homeManagerConfig
        {programs.home-manager.enable = true;}
      ];
    };
  };
}
