# Identity schema declared inside the home-manager evaluator.
#
# Imported only by the home-manager class aggregator. Inside a
# nix-darwin or NixOS system evaluator, identity is declared per-user
# under "dotfiles.users" (see "_host-users.nix") and the system
# constructor mirrors each user's fields into that user's nested
# home-manager evaluator as "dotfiles.identity".
#
# A consumer using "lib.mkHome" in standalone mode assigns
# "dotfiles.identity" directly inside a module passed through the
# "modules" argument; the assignment flows through the home-manager
# evaluator's module-system merge where these fields are read.
{lib, ...}: {
  options.dotfiles.identity = {
    email = lib.mkOption {
      type = lib.types.str;
      description = "The email address to use throughout this flake.";
    };

    fullName = lib.mkOption {
      type = lib.types.str;
      description = "The full name to use throughout this flake.";
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
      description = "The username to use throughout this flake.";
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
          allowed signers file. Defaults to [ identity.email ] if not specified.
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
        Your own identity (using identity.sshSigning.emailAddresses and
        identity.sshSigning.key) is automatically included when sshSigning.key is set.
      '';
      default = [];
    };
  };
}
