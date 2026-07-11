# Identity record for the user defined on this host.
#
# The "./modules/hosts/MyComputer.nix" file imports this attribute set
# as plain data and assigns it to the "dotfiles.identity" option inside
# the home-manager evaluator. The schema for that option is declared by
# the "_user-identity.nix" module, which the home-manager class
# aggregator in the dotfiles flake imports.
#
# Note that the "name" field is required here, unlike on a host that a
# nix-darwin or NixOS configuration manages: a home-manager
# configuration serves one person, so there is no attribute key from
# which to default the user name. The "mkHome" constructor reads this
# field to fill the "home.username" option, and derives the
# "home.homeDirectory" option from it.
#
# Available options:
#   https://github.com/seh/dotfiles/blob/main/modules/_user-identity.nix
{
  email = "seharris@example.com";
  fullName = "Steve Harris";
  gpgKey = "CAFEBABECAFEBABE";
  name = "seharris";
}
