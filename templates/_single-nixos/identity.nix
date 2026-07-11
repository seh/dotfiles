# Identity record for the user defined on this host.
#
# We import this attribute set as plain data by the
# "./modules/hosts/MyComputer.nix" file and assign it to the
# "dotfiles.users.<name>.identity" attribute inside the NixOS
# evaluator. The schema for the "dotfiles.users.<name>" record is
# declared by the "_users.nix" module, which the "_host-users.nix"
# module imports, and the nix-darwin and NixOS class aggregators in
# the dotfiles flake import that one.
#
# Note that the "name" field is intentionally absent: the schema's
# submodule defaults the "identity.name" attribute's value to the
# attribute key under the "dotfiles.users" option, so the user name
# lives at the "dotfiles.users" attribute path in the host file, where
# the attribute namespace enforces uniqueness across users.
#
# Available options:
#   https://github.com/seh/dotfiles/blob/main/modules/_users.nix
{
  email = "seharris@example.com";
  fullName = "Steve Harris";
  gpgKey = "CAFEBABECAFEBABE";
}
