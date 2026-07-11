# Identity values for this flake. Imported as plain data by the
# "./modules/hosts/MyComputer.nix" file and used to populate the
# per-user entry under the "dotfiles.users" registry passed into the
# "inputs.dotfiles.lib.mkDarwin" constructor through the "modules"
# argument so that the target nix-darwin and home-manager evaluators
# apply the assignment. The schema for the "dotfiles.users.<name>"
# record is declared by the "_users.nix" module in the dotfiles flake,
# shared by all three module classes.
#
# Available options:
#   https://github.com/seh/dotfiles/blob/main/modules/_users.nix
{
  email = "seharris@example.com";
  fullName = "Steve Harris";
  gpgKey = "CAFEBABECAFEBABE";
  name = "seharris";
}
