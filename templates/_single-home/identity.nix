# Identity values for this flake. Imported as plain data by the
# "./modules/hosts/MyComputer.nix" file and passed into the
# "inputs.dotfiles.lib.mkHome" constructor through the "modules"
# argument so that the target home-manager evaluator applies the
# assignment. The schema for the "dotfiles.identity" record is
# declared by the "_user-identity.nix" module that the home-manager
# class aggregator in the dotfiles flake imports.
#
# Available options:
#   https://github.com/seh/dotfiles/blob/main/modules/_user-identity.nix
{
  email = "seharris@example.com";
  fullName = "Steve Harris";
  gpgKey = "CAFEBABECAFEBABE";
  name = "seharris";
}
