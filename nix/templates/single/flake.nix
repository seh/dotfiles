{
  description = "dotfiles instantiation";

  inputs = {
    dotfiles.url = "github:seh/dotfiles";
    flake-parts.follows = "dotfiles/flake-parts";
    nixpkgs.follows = "dotfiles/nixpkgs";
  };

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  outputs =
    { self, flake-parts, ... }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } (
      { config, lib, ... }:
      let
        hostName = "MyComputer";
        # Likely alternative: "x86_64-darwin"
        hostPlatform = "aarch64-darwin";
      in
      {
        imports = [
          inputs.dotfiles.flakeModules.default
          inputs.dotfiles.flakeModules.checks # optional
        ];

        dotfiles.user = {
          # Available set of options:
          # https://github.com/seh/dotfiles/blob/main/nix/config.nix
          email = "seharris@example.com";
          fullName = "Steve Harris";
          gpgKey = "CAFEBABECAFEBABE";
          name = "seharris";
        };

        flake =
          let
            basicHomeManagerConfig = {
              # Options are defined in this directory tree:
              # https://github.com/seh/dotfiles/tree/main/nix/home
              dotfiles.profiles = {
                enableAll = true;
                # Further tune more options:
                # development = {
                #   enableKubernetes = true;
                #   enableRust = true;
                # };
              };
              home.stateVersion = "25.11";
            };
          in
          {
            homeConfigurations = {
              "${config.dotfiles.user.name}@${hostName}.local" = self.lib.mkHome {
                pkgs = self.lib.pkgsFor hostPlatform;
                modules = [
                  (
                    basicHomeManagerConfig
                    // {
                      programs.home-manager.enable = true;
                    }
                  )
                ];
              };
            };

            darwinConfigurations =
              let
                darwinConfig = self.lib.mkDarwin {
                  inherit hostPlatform;
                  modules = [
                    {
                      # Options are defined in this directory tree:
                      # https://github.com/seh/dotfiles/tree/main/nix/nix-darwin
                      dotfiles.profiles.apps.enable = true;

                      # Override some default values as necessary:
                      # system.stateVersion = 4; # Default is 6

                      home-manager.users.${config.dotfiles.user.name} = basicHomeManagerConfig;
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

        systems = [
          hostPlatform
        ];
      }
    );
}
