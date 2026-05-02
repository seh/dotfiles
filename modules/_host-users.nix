# System-level multi-user schema for nix-darwin and NixOS hosts.
#
# Each host declares one or more users under "dotfiles.users.<name>".
# Each user contributes a complete, self-contained record:
# identity fields plus cascade inputs (profiles, features,
# excludeProfiles, excludeFeatures). There is no inheritance between
# users; sharing is the consumer's responsibility, expressed via
# plain Nix "let" bindings or shared modules.
#
# The "lix.channel" option is host-wide and shared across all users.
#
# This module is imported by the nix-darwin and NixOS class
# aggregators, not by the home-manager class aggregator. The
# narrowed home-manager identity schema lives in
# "_user-identity.nix".
{
  lib,
  config,
  ...
}: let
  inherit (lib) mkOption types;

  identitySubmoduleFor = userName:
    types.submodule {
      options = {
        email = mkOption {
          type = types.str;
          description = "The email address to use throughout this flake.";
        };

        fullName = mkOption {
          type = types.str;
          description = "The full name to use throughout this flake.";
        };

        gpgKey = mkOption {
          type = types.nullOr types.str;
          description = "The GPG key ID to use for commit signing throughout this flake.";
          default = null;
        };

        commitSigningBackend = mkOption {
          type = types.nullOr (
            types.enum [
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

        name = mkOption {
          type = types.str;
          default = userName;
          description = ''
            The username for this user. Defaults to the attribute
            name under "dotfiles.users".
          '';
        };

        sshSigning = {
          key = mkOption {
            type = types.nullOr types.str;
            description = ''
              The SSH public key to use for commit signing throughout this flake.
              Should be the full public key string (e.g., "ssh-ed25519 AAAAC3...").
            '';
            default = null;
          };

          emailAddresses = mkOption {
            type = types.listOf types.str;
            description = ''
              Email addresses to associate with the SSH signing key in the
              allowed signers file. Defaults to [ identity.email ] if not specified.
            '';
            default = [];
          };
        };

        sshAllowedSigners = mkOption {
          type = types.listOf (
            types.submodule {
              options = {
                email = mkOption {
                  type = types.str;
                  description = "Email address associated with this signer.";
                };
                key = mkOption {
                  type = types.str;
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
    };

  userSubmodule = types.submodule (
    {name, ...}: {
      options = {
        identity = mkOption {
          type = identitySubmoduleFor name;
          default = {};
          description = ''
            Identity fields for this user. Mirrored into the nested
            home-manager evaluator as "config.dotfiles.identity".
          '';
        };

        profiles = mkOption {
          type = types.listOf types.str;
          default = [];
          description = ''
            Profile names this user opts into. These seed the typed
            cascade walk in "flake.lib.expandClosure" inside the
            nested home-manager evaluator for this user.
          '';
        };

        excludeProfiles = mkOption {
          type = types.listOf types.str;
          default = [];
          description = ''
            Profile names to subtract from this user's resolved
            profile closure.
          '';
        };

        features = mkOption {
          type = types.listOf types.str;
          default = [];
          description = ''
            Feature names this user opts into directly (outside of
            any profile that would pull them in).
          '';
        };

        excludeFeatures = mkOption {
          type = types.listOf types.str;
          default = [];
          description = ''
            Feature names to subtract from this user's resolved
            feature closure.
          '';
        };

        homeManagerConfig = mkOption {
          type = types.deferredModule;
          default = {};
          description = ''
            Additional home-manager configuration for this user.
            Merged into "home-manager.users.<name>" inside the
            nix-darwin or NixOS evaluator. Accepts a module
            attrset (option assignments and/or "imports"), a
            module function, or a list of modules.

            Use this to keep a user's complete configuration
            (identity, cascade inputs, and home-manager
            customization) at one option path.
          '';
        };
      };
    }
  );
in {
  options.dotfiles = {
    users = mkOption {
      type = types.attrsOf userSubmodule;
      default = {};
      description = ''
        Per-user records for this host. Each entry's key is the
        username; each value is a complete, self-contained record
        of identity fields and cascade inputs.
      '';
    };

    primaryUser = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        The primary user for this host. On nix-darwin, this is
        propagated to "system.primaryUser" and is required: when
        more than one user is defined, it must be set explicitly;
        when exactly one user is defined and this option is unset,
        it defaults to that user's name. On NixOS and standalone
        home-manager, this option is not enforced.
      '';
    };

    lix.channel = mkOption {
      type = types.strMatching "git|latest|lix_[0-9]+_[0-9]+|stable";
      default = "stable";
      description = ''
        The Lix package set channel to use (e.g. "stable", "latest").
      '';
    };
  };

  config = let
    framework = config.dotfiles.host.framework or null;
    userNames = builtins.attrNames config.dotfiles.users;
  in {
    # On nix-darwin with exactly one configured user, default
    # "primaryUser" to that user's name. With more than one user,
    # the consumer must set "primaryUser" explicitly; the
    # assertion below catches the omission.
    dotfiles.primaryUser = lib.mkIf (framework == "nixDarwin" && builtins.length userNames == 1) (
      lib.mkDefault (builtins.head userNames)
    );

    assertions = lib.optionals (framework == "nixDarwin") [
      {
        assertion = builtins.length userNames <= 1 || config.dotfiles.primaryUser != null;
        message = ''
          dotfiles.primaryUser must be set when more than one user is defined on a nix-darwin host
        '';
      }
    ];
  };
}
