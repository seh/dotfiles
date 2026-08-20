# Declaration of the per-user registry.
#
# This module declares the "dotfiles.users" option and its per-user
# submodule schema, and assigns nothing. Every module class imports
# it, so the registry exists in each one, defaulting to the empty
# attrset. Each entry is a complete, self-contained per-user record:
# identity fields plus the user's selected features and interests and
# their exclusions. There is no inheritance between users; sharing is
# the caller's business, expressed with ordinary Nix "let" bindings or
# shared modules.
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

            When you configure only one of "gpgKey" and
            "sshSigning.key", you may leave this unset and this flake
            infers the backend. When you configure both, set this
            option explicitly.
          '';
        };

        name = mkOption {
          type = types.str;
          default = userName;
          description = ''
            This user's name. When you leave it unset, the module
            system supplies the attribute name this record sits under
            in "dotfiles.users". The propagation module in the
            "modules/lib/_constructors.nix" file mirrors it into this
            user's home-manager evaluator as "dotfiles.identity.name".
            This option does not decide the account name; that module
            keys "users.users" and "home-manager.users" by the
            attribute name instead.
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
              Email addresses to associate with the SSH signing key in
              the allowed signers file. When you list none, the
              "vcs/commit-signing" feature uses "identity.email"
              alone.
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
            Additional SSH signers to trust for commit signature
            verification. When you set "sshSigning.key", this flake
            adds your own identity (from
            "identity.sshSigning.emailAddresses" and
            "identity.sshSigning.key") on its own.
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
            Identity fields for this user. The propagation module in
            the "modules/lib/_constructors.nix" file copies them into
            this user's nested home-manager evaluator as
            "config.dotfiles.identity".
          '';
        };

        features = mkOption {
          type = types.listOf types.str;
          default = [];
          description = ''
            The features this user selects, from the finest single
            concern to a bundle that only brings others along.
            Expansion starts from these selected names together with
            the "interests" list.
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

        interests = mkOption {
          type = types.listOf types.str;
          default = [];
          description = ''
            The interests this user selects: the wants that guard
            contingent features, such as a programming language this
            user works in. Expansion starts from these selected names
            together with the "features" list. A feature name belongs
            in the "features" list instead; the two kinds are not
            interchangeable, and an assertion in the
            "modules/_assertions.nix" file rejects a name written
            under the wrong one.
          '';
        };

        excludeInterests = mkOption {
          type = types.listOf types.str;
          default = [];
          description = ''
            The interests this user deletes from the implication graph
            before that user's activation walk. Withholding an
            interest withholds every contingent feature that has no
            other way to satisfy its preconditions. These entries
            always hold for this user, and they prune what the machine
            provides as well as what this user asked for; the
            machine's own exclusions yield to a user who selects the
            same name.
          '';
        };

        homeManagerConfig = mkOption {
          type = types.deferredModule;
          default = {};
          description = ''
            Additional home-manager configuration for this user. The
            propagation module in the "modules/lib/_constructors.nix"
            file imports it into "home-manager.users.<name>" inside
            the nix-darwin or NixOS evaluator. This option accepts a
            module attrset (option assignments and/or "imports"), a
            module function, or a list of modules.

            Use this to keep a user's complete configuration
            (identity, selected features and interests, and
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
      identity fields plus the user's selected features and interests
      and their exclusions. Meaningful only on hosts that a system
      configuration manages (nix-darwin or NixOS); in the home-manager
      class the registry must stay empty.
    '';
  };
}
