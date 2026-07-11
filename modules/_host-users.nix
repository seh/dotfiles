# System-level multi-user options for nix-darwin and NixOS hosts.
#
# The per-user registry itself—the "dotfiles.users" option—is
# declared in shared scope by the "_users.nix" module, so that the
# activation substrate in "_activation.nix" can compute the per-user
# activation union in every class. This module carries the options
# that are meaningful only where a system configuration manages the
# host: the primary-user designation and the host-wide Lix channel,
# shared across all users.
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
