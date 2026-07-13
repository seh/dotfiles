{
  inputs,
  lib,
  ...
}: let
  hostName = "MyComputer";
  # Likely alternative: "x86_64-linux".
  hostPlatform = "aarch64-linux";
in {
  flake.nixosConfigurations = let
    identity = import ../../identity.nix;
    username = identity.name;

    nixosConfig = inputs.dotfiles.lib.mkNixOS {
      inherit hostPlatform;
      modules = [
        # This host runs as an LXC container, of the sort OrbStack
        # provides. The module system supplies "modulesPath" as the
        # path to the NixOS modules directory inside nixpkgs.
        #
        # A container shares the host's kernel. For a system that
        # boots its own kernel instead, whether a physical computer or
        # a virtual machine, drop the "imports" entry below and supply
        # the two things NixOS then requires that this template cannot
        # know: a root filesystem and a boot loader. Generate a
        # "hardware-configuration.nix" file on that machine with the
        # "nixos-generate-config" tool, import it here in place of the
        # container profile, and choose a boot loader to suit the
        # machine's firmware, such as the "boot.loader.systemd-boot"
        # or "boot.loader.grub" options.
        ({modulesPath, ...}: {
          imports = [
            "${modulesPath}/virtualisation/lxc-container.nix"
          ];

          networking.hostName = hostName;

          system = {
            # Set once at installation time; do not change.
            stateVersion = "26.11";
          };

          dotfiles = {
            host = {
              name = hostName;
            };
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
        })
      ];
    };
  in
    {
      ${hostName} = nixosConfig;
    }
    # Alias for convenience when rebuilding locally, unless this host
    # already answers to that name.
    // lib.optionalAttrs (hostName != "local") {
      local = nixosConfig;
    };
}
