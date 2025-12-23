_:
{ lib, ... }:

#let
#  TODO(seh): Use this if we need to refer to any of the current
#  values for these options.
#  cfg = flake.config.dotfiles;
#  nixosRelease = localFlake.inputs.nixos.lib.trivial.release;
#in
{
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
        type = lib.types.str;
        description = "The GPG key ID to use for commit signing throughout this flake.";
        default = "";
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
          type = lib.types.str;
          description = ''
            The SSH public key to use for commit signing throughout this flake.
            Should be the full public key string (e.g., "ssh-ed25519 AAAAC3...").
          '';
          default = "";
        };

        emailAddresses = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          description = ''
            Email addresses to associate with the SSH signing key in the
            allowed signers file. Defaults to [ user.email ] if not specified.
          '';
          default = [ ];
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
        default = [ ];
      };
    };

    darwin.modules = lib.mkOption {
      type = with lib.types; listOf deferredModule;
      default = [ ];
      description = ''
        Default list of modules to include when generating nix-darwin
        configuration with `lib.mkDarwin` and `lib.importDarwin`.
      '';
    };

    home.modules = lib.mkOption {
      type = with lib.types; listOf deferredModule;
      default = [ ];
      description = ''
        Default list of modules to include when generating Home Manager
        configuration with `lib.mkHome` and `lib.importHome`.
      '';
    };

    nix.package = lib.mkOption {
      type = lib.types.str;
      description = "The Nix package to use, specified by attribute name.";
      default = "latest"; # Example alternative: "nix_2_24"
    };

    nixos.modules = lib.mkOption {
      type = with lib.types; listOf deferredModule;
      default = [ ];
      description = ''
        Default list of modules to include when generating NixOS configuration
        with `lib.mkNixOS` and `lib.importNixOS`.
      '';
    };
  };
}
