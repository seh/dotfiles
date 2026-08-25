# The primary-user designation, as nix-darwin alone requires it.
#
# The "system.primaryUser" option is a nix-darwin concept with no
# counterpart in the NixOS or standalone home-manager evaluators, so
# the nix-darwin class aggregator imports this module alone. Being
# imported at all settles which class evaluates here, which is why the
# default and the assertion below stand unguarded.
#
# The "dotfiles.primaryUser" option itself is declared in the
# "_host-users.nix" file, which every system class aggregator imports,
# and the per-user registry it reads is declared in the "_users.nix"
# file. This module declares only what nix-darwin makes of the two.
{
  lib,
  config,
  ...
}: let
  userNames = builtins.attrNames config.dotfiles.users;
in {
  # With exactly one configured user, default the "primaryUser" option
  # to that user's name. With more than one user, the consumer must
  # set the "primaryUser" option explicitly; the assertion below
  # catches the omission.
  dotfiles.primaryUser = lib.mkIf (builtins.length userNames == 1) (
    lib.mkDefault (builtins.head userNames)
  );

  assertions = [
    {
      assertion = builtins.length userNames <= 1 || config.dotfiles.primaryUser != null;
      message = ''
        dotfiles.primaryUser must be set when more than one user is
        defined on a nix-darwin host
      '';
    }
  ];
}
