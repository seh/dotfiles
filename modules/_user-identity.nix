# Shared identity and Lix-channel schema used across the home,
# darwin, and nixos class aggregators.
#
# Declaring these options inside the class aggregators (rather than
# at the flake-parts level) means a consumer assigns them inside a
# module it passes to "lib.mkHome", "lib.mkDarwin", or "lib.mkNixOS"
# — that assignment flows through the module-system merge inside
# the home-manager, nix-darwin, or NixOS evaluator where the options
# actually get read.
{lib, ...}: {
  options.dotfiles = {
    user = {
      email = lib.mkOption {
        type = lib.types.str;
        description = "The default email address to use throughout this flake.";
        default = "seh@panix.com";
      };

      fullName = lib.mkOption {
        type = lib.types.str;
        description = "The default full name to use throughout this flake.";
        default = "Steven E. Harris";
      };

      gpgKey = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        description = "The GPG key ID to use for commit signing throughout this flake.";
        default = null;
      };

      commitSigningBackend = lib.mkOption {
        type = lib.types.nullOr (
          lib.types.enum [
            "gpg"
            "ssh"
          ]
        );
        default = null;
        description = ''
          The signing backend to use for commits: "gpg" or "ssh".

          When only one of gpgKey or sshSigning.key is configured, this
          can be left unset and the backend will be inferred. When both
          are configured, this option must be set explicitly.
        '';
      };

      name = lib.mkOption {
        type = lib.types.str;
        description = "The default username to use throughout this flake.";
        default = "seh";
      };

      sshSigning = {
        key = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          description = ''
            The SSH public key to use for commit signing throughout this flake.
            Should be the full public key string (e.g., "ssh-ed25519 AAAAC3...").
          '';
          default = null;
        };

        emailAddresses = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          description = ''
            Email addresses to associate with the SSH signing key in the
            allowed signers file. Defaults to [ user.email ] if not specified.
          '';
          default = [];
        };
      };

      sshAllowedSigners = lib.mkOption {
        type = lib.types.listOf (
          lib.types.submodule {
            options = {
              email = lib.mkOption {
                type = lib.types.str;
                description = "Email address associated with this signer.";
              };
              key = lib.mkOption {
                type = lib.types.str;
                description = "SSH public key string for this signer.";
              };
            };
          }
        );
        description = ''
          Additional SSH signers to trust for commit signature verification.
          Your own identity (using user.sshSigning.emailAddresses and
          user.sshSigning.key) is automatically included when sshSigning.key is set.
        '';
        default = [];
      };
    };

    lix.channel = lib.mkOption {
      type = lib.types.strMatching "git|latest|lix_[0-9]+_[0-9]+|stable";
      default = "stable";
      description = ''
        The Lix package set channel to use (e.g. "stable", "latest").
      '';
    };
  };
}
