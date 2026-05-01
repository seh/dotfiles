# Identity values for this flake. Imported as plain data by
# "./modules/hosts/MyComputer.nix" and passed into
# "inputs.dotfiles.lib.mkHome" through the "modules" argument so
# that the target home-manager evaluator applies the assignment.
# The schema for "dotfiles.user" is declared by the
# "_user-identity.nix" module that each of the home-manager,
# nix-darwin, and NixOS class aggregators in the dotfiles flake
# imports.
#
# Available options:
#   https://github.com/seh/dotfiles/blob/main/modules/_user-identity.nix
{
  dotfiles.user = {
    email = "seharris@example.com";
    fullName = "Steve Harris";
    gpgKey = "CAFEBABECAFEBABE";
    name = "seharris";
  };
}
