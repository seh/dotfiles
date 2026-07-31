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
            # This machine's own selections decide its own
            # configuration—every "nixOS" body follows these alone—and
            # are layered into every managed user's home, so the
            # machine provisions each user it manages. Select here
            # what every account on this machine should have: the
            # "essential" profile implies the features that configure
            # the machine itself.
            host = {
              name = hostName;
              profiles = [
                "essential"
              ];
              features = [
                # "compat/bazel-fhs"
              ];
            };
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
