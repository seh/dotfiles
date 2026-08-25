# The operating-system accounts that NixOS alone insists upon.
#
# Every entry under the "dotfiles.users" registry is a user this
# machine manages, so the machine owes that user an account. NixOS
# restricts an account to rules that nix-darwin does not—it must
# declare itself either a normal user or a system user, and it must
# belong to a group—and the options expressing those rules exist in
# NixOS alone, so the NixOS class aggregator imports this module by
# itself. It sits beside the "_darwin-primary-user.nix" file, which
# declares what nix-darwin makes of the same registry. Being imported
# at all settles which class evaluates here, which is why the
# assignment below stands unguarded.
#
# The propagation module in the "modules/lib/_constructors.nix" file
# serves both system classes, so it declares only what the two share:
# the account's home directory, and the user's nested home-manager
# evaluator. This module supplies the remainder here.
{
  lib,
  config,
  ...
}: {
  # Declare each managed user an ordinary person's account. NixOS
  # draws the group membership, the login shell, and the creation of
  # the home directory from that one declaration, so it is all an
  # account needs to stand. The assignment is a default, leaving a
  # consumer free to recast a particular user—as a system account,
  # say—under the "users.users.<name>" option.
  users.users =
    lib.mapAttrs (_userName: _userRecord: {
      isNormalUser = lib.mkDefault true;
    })
    config.dotfiles.users;
}
