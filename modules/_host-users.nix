# System-level multi-user options for nix-darwin and NixOS hosts.
#
# The per-user registry itself—the "dotfiles.users" option—is
# declared in shared scope by the "_users.nix" module, so that the
# activation substrate in "_activation.nix" can compute the per-user
# activation union in every class. This module carries the options
# that are meaningful only where a system configuration manages the
# host: the primary-user designation and the host-wide Lix channel,
# shared across all users. What nix-darwin requires of that
# designation—a default drawn from a sole user, and an explicit
# assignment once there are several—lives in
# "_darwin-primary-user.nix", which the nix-darwin class aggregator
# alone imports.
#
# This module is imported by the nix-darwin and NixOS class
# aggregators, not by the home-manager class aggregator. The
# narrowed home-manager identity schema lives in
# "_user-identity.nix".
{lib, ...}: let
  inherit (lib) mkOption types;
in {
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
}
