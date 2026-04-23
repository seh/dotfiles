# Activation substrate shared by Home Manager, nix-darwin, and NixOS
# configurations.
#
# This module declares the options that describe hosts and the
# currently-active host's resolved record, plus the set of "known
# profile and feature names" that profile and feature modules
# advertise. It is imported by each of
# "flake.modules.homeManager.default", "flake.modules.darwin.default",
# and "flake.modules.nixos.default".
{
  lib,
  config,
  ...
}: let
  inherit (lib) mkOption types;

  flakeLib = config.dotfiles._flakeLib;
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
            profiles = mkOption {
              type = types.listOf types.str;
              default = [];
              description = "Profile names this host opts into.";
            };
            features = mkOption {
              type = types.listOf types.str;
              default = [];
              description = "Feature names this host opts into.";
            };
          };
        }
      );
      default = {};
      description = ''
        Declarative registry of all hosts managed by this flake or a
        downstream instantiation flake. Each entry names a host and
        records its framework, platform, and profile/feature activations.
      '';
    };

    _host = mkOption {
      type = types.submodule (
        submoduleArgs: let
          hostCfg = submoduleArgs.config;
        in {
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
            profiles = mkOption {
              type = types.listOf types.str;
              default = [];
              description = ''
                Profile names this host opts into. These seed the
                typed cascade walk in "flake.lib.expandClosure",
                which produces "activeProfiles" and
                "activeFeatures".
              '';
            };
            features = mkOption {
              type = types.listOf types.str;
              default = [];
              description = ''
                Feature names this host opts into directly (outside
                of any profile that would pull them in). These seed
                the typed cascade walk together with "profiles".
              '';
            };
            excludeProfiles = mkOption {
              type = types.listOf types.str;
              default = [];
              description = ''
                Profile names to subtract from this host's resolved
                profile closure. Useful for temporarily disabling a
                profile without editing the host record.
              '';
            };
            excludeFeatures = mkOption {
              type = types.listOf types.str;
              default = [];
              description = ''
                Feature names to subtract from this host's resolved
                feature closure. Useful for temporarily disabling a
                feature without editing the host record.
              '';
            };
            activeProfiles = mkOption {
              type = types.listOf types.str;
              readOnly = true;
              description = ''
                Profiles in effect after expanding "profiles" through
                the typed cascade and subtracting "excludeProfiles".
              '';
            };
            activeFeatures = mkOption {
              type = types.listOf types.str;
              readOnly = true;
              description = ''
                Features in effect after expanding "profiles" and
                "features" through the typed cascade and subtracting
                "excludeFeatures".
              '';
            };
            activatesProfile = mkOption {
              type = types.functionTo types.bool;
              readOnly = true;
              description = ''
                Predicate testing whether a profile name is present
                in "activeProfiles".
              '';
            };
            activatesFeature = mkOption {
              type = types.functionTo types.bool;
              readOnly = true;
              description = ''
                Predicate testing whether a feature name is present
                in "activeFeatures".
              '';
            };
            unreachableProfiles = mkOption {
              type = types.listOf types.str;
              readOnly = true;
              description = ''
                Profiles advertised via "dotfiles._knownProfiles"
                that this host's resolved closure ("activeProfiles")
                does not include. Exposed as a diagnostic aid.
              '';
            };
            unreachableFeatures = mkOption {
              type = types.listOf types.str;
              readOnly = true;
              description = ''
                Features advertised via "dotfiles._knownFeatures"
                that this host's resolved closure ("activeFeatures")
                does not include. Exposed as a diagnostic aid.
              '';
            };
          };

          config = let
            expanded =
              if flakeLib != null && flakeLib ? expandClosure && flakeLib ? cascadesFor
              then
                flakeLib.expandClosure
                (flakeLib.cascadesFor {
                  inherit (hostCfg) framework;
                  isDarwin = hostCfg.framework == "nixDarwin";
                })
                {
                  profiles = config.dotfiles._knownProfiles;
                  features = config.dotfiles._knownFeatures;
                }
                {
                  inherit (hostCfg) profiles features;
                }
              else {
                inherit (hostCfg) profiles features;
              };
            activeProfiles = lib.subtractLists hostCfg.excludeProfiles expanded.profiles;
            activeFeatures = lib.subtractLists hostCfg.excludeFeatures expanded.features;
          in {
            inherit activeProfiles activeFeatures;
            activatesProfile = name: builtins.elem name activeProfiles;
            activatesFeature = name: builtins.elem name activeFeatures;
          };
        }
      );
      default = {};
      description = ''
        The current host's resolved record. Set internally by
        "lib.mkHome", "lib.mkDarwin", and "lib.mkNixOS".
      '';
    };

    _knownProfiles = mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        Profile names that imported profile modules declare they
        respond to. Accumulated via "listOf"'s append-merge
        semantics. Used to catch typos in a host's declared
        "profiles" list and to diagnose role mismatches.
      '';
    };

    _knownFeatures = mkOption {
      type = types.listOf types.str;
      default = [];
      description = ''
        Feature names that imported feature or profile modules
        declare they respond to. Accumulated via "listOf"'s
        append-merge semantics. Used to catch typos in a host's
        declared "features" list and to diagnose role mismatches.
      '';
    };

    _flakeLib = mkOption {
      type = with types; nullOr (lazyAttrsOf raw);
      default = null;
      internal = true;
      description = ''
        Proxy into "flake.lib" for use by this module's typed
        activation closure ("activeProfiles", "activeFeatures").
        Populated by each class aggregator. Kept nullable so that
        direct instantiations of this module (e.g. for tests) remain
        possible without a flake-parts context.
      '';
    };
  };

  config.dotfiles._host = {
    unreachableProfiles = lib.subtractLists config.dotfiles._host.activeProfiles config.dotfiles._knownProfiles;
    unreachableFeatures = lib.subtractLists config.dotfiles._host.activeFeatures config.dotfiles._knownFeatures;
  };
}
