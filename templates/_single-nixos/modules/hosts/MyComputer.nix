{inputs, ...}: let
  hostName = "MyComputer";
  # Likely alternative: "x86_64-linux"
  hostPlatform = "aarch64-linux";
in {
  flake.nixosConfigurations = let
    identity = import ../../identity.nix;
    username = identity.name;

    nixosConfig = inputs.dotfiles.lib.mkNixOS {
      inherit hostPlatform;
      modules = [
        # If this host runs as an LXC container (for example
        # under OrbStack), declare its container profile by
        # adding "modulesPath" to this module's argument set and
        # adding an "imports" entry alongside the other top-level
        # attributes (e.g., next to "networking" below). That is:
        #
        #   ({modulesPath, ...}: {
        #     imports = [
        #       "${modulesPath}/virtualisation/lxc-container.nix"
        #     ];
        #     networking.hostName = hostName;
        #     ...
        #   })
        #
        # The module-system supplies "modulesPath" as the path to
        # the NixOS modules directory inside nixpkgs. It is left
        # out here because the LXC profile does not apply to
        # bare-metal or virtual-machine NixOS installations.
        (_: {
          networking.hostName = hostName;

          system = {
            # Set once at installation time; do not change.
            stateVersion = "25.05";
          };

          dotfiles = {
            host = {
              name = hostName;
              framework = "nixOS";
              platform = hostPlatform;
            };
            # Activate feature modules by listing profiles and
            # features that apply to this user. The "all" profile
            # is an umbrella that expands to every profile this
            # flake advertises, computed from the "knownProfiles"
            # registry. To opt out of specific umbrella members
            # (such as the Firefox/Safari customization in "web"),
            # list them under "excludeProfiles". Add specific
            # features such as "kubernetes", "cloud/aws", or
            # "lang/rust" under "features". See:
            # https://github.com/seh/dotfiles/tree/main/modules/home/profiles
            # https://github.com/seh/dotfiles/tree/main/modules/home/features
            users.${username} = {
              inherit identity;
              profiles = [
                "all"
              ];
              # Opt out of specific umbrella members. For example,
              # to skip Firefox/Safari customization:
              # excludeProfiles = ["web"];
              features = [
                # "kubernetes"
                # "cloud/aws"
                # "lang/rust"
              ];
              homeManagerConfig = {
                home.stateVersion = "25.05";
              };
            };
          };
        })
      ];
    };
  in {
    ${hostName} = nixosConfig;
    # Alias for convenience when rebuilding locally.
    local = nixosConfig;
  };
}
