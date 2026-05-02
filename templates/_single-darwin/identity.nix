# Identity values for this flake. Imported as plain data by
# "./modules/hosts/MyComputer.nix" and used to seed the per-user
# entry under "dotfiles.users" passed into
# "inputs.dotfiles.lib.mkDarwin" through the "modules" argument so
# that the target nix-darwin and home-manager evaluators apply the
# assignment. The schema for "dotfiles.users.<name>" is declared by
# the "_host-users.nix" module that the nix-darwin and NixOS class
# aggregators in the dotfiles flake import.
#
# Available options:
#   https://github.com/seh/dotfiles/blob/main/modules/_host-users.nix
{
  email = "seharris@example.com";
  fullName = "Steve Harris";
  gpgKey = "CAFEBABECAFEBABE";
  name = "seharris";
}
