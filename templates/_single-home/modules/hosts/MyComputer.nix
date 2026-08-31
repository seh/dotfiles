{
  inputs,
  lib,
  ...
}: let
  hostName = "MyComputer";
  # Likely alternatives: "aarch64-linux", "x86_64-linux".
  hostPlatform = "aarch64-darwin";

  identity = import ../../identity.nix;
in {
  flake.homeConfigurations = let
    # Instantiate the "pkgs" package set once for this host so that
    # adding further users below reuses the same package set rather
    # than instantiating nixpkgs again per user. The "mkHome" function
    # extends whatever package set it receives with the dotfiles
    # flake's own nixpkgs overlay, so withhold that overlay here;
    # asking for it in both places applies it twice.
    pkgs = inputs.dotfiles.lib.pkgsFor {
      system = hostPlatform;
      applyOverlays = false;
    };

    # Per-user records for this host. Each entry's key is the
    # username; each value contains that user's identity record and
    # the list of modules supplied to "mkHome".
    #
    # To add a second user, add another entry here with its own
    # identity (imported from a separate file or assigned inline) and
    # module list. The "flake.homeConfigurations" attribute below
    # expands each entry to a "<user>@<host>" configuration name.
    users = {
      ${identity.name} = {
        inherit identity;
        modules = [
          {
            # Set once at installation time; do not change.
            home.stateVersion = "26.11";

            dotfiles = {
              # Activate feature modules by listing the features that
              # apply to this user, from a bundle that only brings
              # others along to the finest single concern. The "all"
              # feature implies every body-less bundle this flake
              # advertises. To opt out of specific members of the
              # "all" feature (such as the Firefox customization in
              # the "web" feature), list them under the
              # "excludeFeatures" list. Add narrower features such as
              # "kubernetes" or "cloud/aws" to the same list, and
              # interests such as "lang/rust"—the languages in which
              # this user works—under the "interests" list. See:
              # https://github.com/seh/dotfiles/tree/main/modules/home/features
              host = {
                name = hostName;
                features = [
                  "all"
                  # "kubernetes"
                  # "cloud/aws"
                ];
                # Opt out of specific members of the "all" feature.
                # For example, to skip Firefox customization:
                # excludeFeatures = ["web"];
                interests = [
                  # "lang/rust"
                ];
              };
            };

            # Add further Home Manager modules or option assignments
            # here, such as "programs.git.enable = true;" or imports
            # of locally defined modules.
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
