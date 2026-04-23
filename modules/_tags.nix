# Tag substrate shared by Home Manager, nix-darwin, and NixOS
# configurations.
#
# This module declares the options that describe hosts and the
# currently-active host's resolved record, plus the set of "known
# tags" that feature modules advertise. It is imported by each of
# "flake.homeModules.default", "flake.darwinModules.default", and
# "flake.nixosModules.default".
{lib, ...}: let
  inherit (lib) mkOption types;
in {
  options.dotfiles = {
    hosts = mkOption {
      type = types.attrsOf (
        types.submodule {
          options = {
            framework = mkOption {
              type = types.enum [
                "homeManager"
                "nixDarwin"
                "nixOS"
              ];
              description = "Which configuration framework this host uses.";
            };
            platform = mkOption {
              type = types.str;
              description = "Nixpkgs system string, e.g. \"aarch64-darwin\".";
            };
            tags = mkOption {
              type = types.listOf types.str;
              default = [];
              description = "Feature tags this host opts into.";
            };
          };
        }
      );
      default = {};
      description = ''
        Declarative registry of all hosts managed by this flake or a
        downstream instantiation flake. Each entry names a host and
        records its framework, platform, and feature tags.
      '';
    };

    _host = mkOption {
      type = types.submodule (
        {config, ...}: {
          options = {
            name = mkOption {
              type = types.nullOr types.str;
              default = null;
              description = "Resolved host name, or null when unset.";
            };
            framework = mkOption {
              type = types.nullOr (
                types.enum [
                  "homeManager"
                  "nixDarwin"
                  "nixOS"
                ]
              );
              default = null;
            };
            platform = mkOption {
              type = types.nullOr types.str;
              default = null;
            };
            tags = mkOption {
              type = types.listOf types.str;
              default = [];
              description = "Tags declared for this host.";
            };
            excludeTags = mkOption {
              type = types.listOf types.str;
              default = [];
              description = ''
                Tags to subtract from this host's tag set. Useful for
                temporarily disabling a tagged feature without editing
                the host record.
              '';
            };
            effectiveTags = mkOption {
              type = types.listOf types.str;
              readOnly = true;
              description = ''
                Tags in effect after subtracting "excludeTags" from
                "tags".
              '';
            };
            hasTag = mkOption {
              type = types.functionTo types.bool;
              readOnly = true;
              description = ''
                Predicate testing whether a tag is present in
                "effectiveTags".
              '';
            };
          };

          config = {
            effectiveTags = lib.subtractLists config.excludeTags config.tags;
            hasTag = tag: builtins.elem tag config.effectiveTags;
          };
        }
      );
      default = {};
      description = ''
        The current host's resolved record. Set internally by
        "lib.mkHome", "lib.mkDarwin", and "lib.mkNixOS".
      '';
    };

    _knownTags = mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        Tags that imported feature modules declare they respond to.
        Accumulated via "listOf"'s append-merge semantics. Used to
        catch typos in a host's declared tag list.
      '';
    };
  };
}
