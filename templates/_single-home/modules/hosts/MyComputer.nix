{
  inputs,
  lib,
  ...
}: let
  hostName = "MyComputer";
  # Likely alternatives: "aarch64-darwin", "x86_64-darwin",
  # "x86_64-linux".
  hostPlatform = "aarch64-linux";

  identity = import ../../identity.nix;
in {
  flake.homeConfigurations = let
    # Instantiate "pkgs" once for this host so that adding
    # further users below reuses the same package set rather
    # than instantiating nixpkgs again per user.
    pkgs = inputs.dotfiles.lib.pkgsFor hostPlatform;

    # Per-user records for this host. Each entry's key is the
    # username; each value carries that user's identity record
    # and the list of modules supplied to "mkHome".
    #
    # To add a second user, add another entry here with its own
    # identity (imported from a separate file or assigned
    # inline) and module list. The "flake.homeConfigurations"
    # attribute below expands each entry to a "<user>@<host>"
    # configuration name.
    users = {
      ${identity.name} = {
        inherit identity;
        modules = [
          {
            # Set once at installation time; do not change.
            home.stateVersion = "25.11";

            dotfiles = {
              # Activate feature modules by listing profiles
              # and features that apply to this user. The "all"
              # profile is an umbrella that expands to every
              # profile this flake advertises, computed from
              # the "knownProfiles" registry. To opt out of
              # specific umbrella members (such as the
              # Firefox/Safari customization in "web"), list
              # them under "excludeProfiles". Add specific
              # features such as "kubernetes", "cloud/aws", or
              # "lang/rust" under "features". See:
              # https://github.com/seh/dotfiles/tree/main/modules/home/profiles
              # https://github.com/seh/dotfiles/tree/main/modules/home/features
              host = {
                name = hostName;
                framework = "homeManager";
                platform = hostPlatform;
                profiles = [
                  "all"
                ];
                # Opt out of specific umbrella members. For
                # example, to skip Firefox/Safari customization:
                # excludeProfiles = ["web"];
                features = [
                  # "kubernetes"
                  # "cloud/aws"
                  # "lang/rust"
                ];
              };
            };

            # Add further Home Manager modules or option
            # assignments here, such as "programs.git.enable =
            # true;" or imports of locally defined modules.
          }
        ];
      };
    };

    mkUserConfig = userRecord:
      inputs.dotfiles.lib.mkHome {
        inherit pkgs;
        modules =
          [
            {dotfiles.identity = userRecord.identity;}
          ]
          ++ userRecord.modules;
      };
  in
    lib.mapAttrs' (
      userName: userRecord: lib.nameValuePair "${userName}@${hostName}" (mkUserConfig userRecord)
    )
    users;
}
