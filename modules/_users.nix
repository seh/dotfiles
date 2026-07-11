# Declaration of the per-user registry.
#
# This module declares the "dotfiles.users" option and its per-user
# submodule schema, and assigns nothing. It lives in shared scope so
# that the registry exists in every module class, defaulting to the
# empty attrset. Each entry is a complete, self-contained per-user
# record: identity fields plus the user's selected profiles and
# features and their exclusions. There is no inheritance between
# users; sharing is the caller's business, expressed with ordinary
# Nix "let" bindings or shared modules.
#
# Only a system configuration acts on the entries. A standalone
# home-manager configuration must leave the registry empty.
{lib, ...}: let
  inherit (lib) mkOption types;

  identitySubmoduleFor = userName:
    types.submodule {
      options = {
        email = mkOption {
          type = types.str;
          description = ''
            The email address to use throughout this flake.
          '';
        };

        fullName = mkOption {
          type = types.str;
          description = "The full name to use throughout this flake.";
        };

        gpgKey = mkOption {
          type = types.nullOr types.str;
          description = ''
            The GPG key ID to use for commit signing throughout this
            flake.
          '';
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
              This user's SSH public key for signing commits. Write
              the whole key line, the algorithm name and the base64
              body together, as an OpenSSH ".pub" file contains it.
              The "vcs/commit-signing" feature copies that line into
              the "allowed_signers" file it writes. When the signing
              backend is "ssh", the "vcs/git" and "vcs/jujutsu"
              features configure Git and Jujutsu to sign with this
              key.
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
                  description = ''
                    Email address associated with this signer.
                  '';
                };
                key = mkOption {
                  type = types.str;
                  description = ''
                    SSH public key string for this signer.
                  '';
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
            Profile names this user opts into. Expansion inside
            the nested home-manager evaluator for this user starts
            from these selected names.
          '';
        };

        excludeProfiles = mkOption {
          type = types.listOf types.str;
          default = [];
          description = ''
            Profile names to subtract from this user's resolved
            activation.
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
            The features this user deletes from the implication graph
            before that user's activation walk. The walk drops each
            excluded vertex with its out-edges and its in-edges, so a
            feature reachable only through an excluded one drops out
            as well. These entries always apply to this user; the
            machine's own exclusions yield to a user who selects the
            same name.
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
            (identity, selected profiles and features, and
            home-manager customization) at one option path.
          '';
        };
      };
    }
  );
in {
  options.dotfiles.users = mkOption {
    type = types.attrsOf userSubmodule;
    default = {};
    description = ''
      Per-user records for this host. Each entry's key is the
      username; each value is a complete, self-contained record of
      identity fields plus the user's selected profiles and
      features and their exclusions. Meaningful only on hosts that
      a system configuration manages (nix-darwin or NixOS); in the
      home-manager class the registry must stay empty.
    '';
  };
}
