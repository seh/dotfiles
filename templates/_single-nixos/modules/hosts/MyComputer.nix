{inputs, ...}: let
  hostName = "MyComputer";
  # Likely alternative: "x86_64-linux"
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
    framework = "nixOS";
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
    home.stateVersion = "25.05";
  };

  # If this host runs as an LXC container (for example under
  # OrbStack), add the corresponding NixOS profile to "imports"
  # below:
  #   imports = ["${modulesPath}/virtualisation/lxc-container.nix"];
  # That import is left out here because it does not apply to
  # bare-metal or virtual-machine NixOS installations.
  systemModule = {...}: {
    networking.hostName = hostName;

    system = {
      # Set once at installation time; do not change.
      stateVersion = "25.05";
    };

    home-manager.users.${username} = homeManagerConfig;
  };
in {
  flake.nixosConfigurations = let
    nixosConfig = inputs.dotfiles.lib.mkNixOS {
      inherit hostPlatform;
      modules = [
        {dotfiles.host = hostRecord;}
        identity
        systemModule
      ];
    };
  in {
    ${hostName} = nixosConfig;
    # Alias for convenience when rebuilding locally.
    local = nixosConfig;
  };
}
