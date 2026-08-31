# System-level multi-user options for nix-darwin and NixOS hosts.
#
# The per-user registry itself—the "dotfiles.users" option—is declared
# by the "_users.nix" module, which this module imports, so that the
# registry is visible in the two system classes and nowhere else. This
# module declares the options that are meaningful only where a system
# configuration manages the host: the primary-user designation and the
# host-wide Lix channel, shared across all users. What nix-darwin
# requires of that designation—a default drawn from a sole user, and
# an explicit assignment once there are several—lives in
# "_darwin-primary-user.nix", which the nix-darwin class aggregator
# alone imports.
#
# This module is imported by the nix-darwin and NixOS class
# aggregators, not by the home-manager class aggregator, so it is also
# where a diagnosis addressed to the machine alone belongs: the
# backstop warning below fires for a machine that manages users while
# selecting nothing of its own. The narrowed home-manager identity
# schema lives in the "_user-identity.nix" file.
{
  lib,
  config,
  ...
}: let
  inherit (lib) mkOption types;
  inherit (import ./lib/_diagnostics.nix) describeHost;
in {
  imports = [./_users.nix];

  options.dotfiles = {
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
    inherit (config.dotfiles) host;
    userNames = builtins.attrNames config.dotfiles.users;
    # Every list through which this machine selects for its own sake.
    # A system-class body follows these alone, so both left empty
    # means the machine applies no system-class feature at all.
    machineSelections = host.features ++ host.interests;
  in {
    # A machine that manages users while selecting nothing of its own
    # withholds every system-class feature, since a system body
    # follows the machine's own selections alone. A consumer who
    # writes all the selections under the "dotfiles.users" registry
    # arrives there without noticing, so this warning states it. Such
    # a machine is legitimate—one that exists only to provision its
    # users' homes—which is why this is a warning rather than an
    # error.
    warnings = lib.optional (userNames != [] && machineSelections == []) ''
      Resolving ${describeHost host.name}: "dotfiles.users" lists ${lib.concatMapStringsSep ", " (n: "\"${n}\"") userNames}, but this machine selects nothing of its own, so it applies no system-class feature. A machine's system configuration follows only "dotfiles.host.features" and "dotfiles.host.interests", never its users' selections. If you meant to configure the machine, add its selections to those lists. If this machine exists only to provision its users' homes, you can ignore this warning.
    '';
  };
}
