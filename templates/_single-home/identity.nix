# Identity values for this flake. Imported as plain data by
# "./modules/hosts/MyComputer.nix" and passed into
# "inputs.dotfiles.lib.mkHome" through the "modules" argument so
# that the target home-manager evaluator applies the assignment.
# The schema for "dotfiles.identity" is declared by the
# "_user-identity.nix" module that the home-manager class
# aggregator in the dotfiles flake imports.
#
# Available options:
#   https://github.com/seh/dotfiles/blob/main/modules/_user-identity.nix
{
  email = "seharris@example.com";
  fullName = "Steve Harris";
  gpgKey = "CAFEBABECAFEBABE";
  name = "seharris";
}
