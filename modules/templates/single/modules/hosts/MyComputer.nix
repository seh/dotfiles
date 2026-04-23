{
  self,
  config,
  lib,
  ...
}: let
  hostName = "MyComputer";
  # Likely alternative: "x86_64-darwin"
  hostPlatform = "aarch64-darwin";

  # Activate feature modules by listing the tags that apply to this
  # host. The "all" tag is a convenience aggregate that expands (via
  # the cascade table in the dotfiles flake) to
  # "essential", "development", "desktop", "fonts", and (on Darwin)
  # "macos". Add specific leaf tags such as "kubernetes", "aws", or
  # "rust" as appropriate. See:
  # https://github.com/seh/dotfiles/tree/main/modules/home/features/profiles
  hostRecord = {
    name = hostName;
    framework = "nixDarwin";
    platform = hostPlatform;
    tags = [
      "all"
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
      "${config.dotfiles.user.name}@${hostName}.local" = self.lib.mkHome {
        pkgs = self.lib.pkgsFor hostPlatform;
        host = hostRecord;
        modules = [
          homeManagerConfig
          {programs.home-manager.enable = true;}
        ];
      };
    };

    darwinConfigurations = let
      darwinConfig = self.lib.mkDarwin {
        inherit hostPlatform;
        pkgs = self.lib.pkgsFor hostPlatform;
        host = hostRecord;
        modules = [
          {
            # nix-darwin feature tags are advertised under:
            # https://github.com/seh/dotfiles/tree/main/modules/nix-darwin/features
            dotfiles._host.tags = ["apps"];

            # Override some default values as necessary:
            # system.stateVersion = 4; # Default is 6

            home-manager.users.${config.dotfiles.user.name} = homeManagerConfig;
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
