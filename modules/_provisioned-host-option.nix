# The read-only declaration of the "dotfiles.host" option, for the
# home-manager evaluator a system configuration builds for each user
# it provisions. The propagation module in the
# "modules/lib/_constructors.nix" file defines the option exactly once
# per managed user: the machine's record, layered with that user's own
# selections and exclusions. The module system then stops the build on
# any further definition, whatever its priority. This declaration
# states no default, which is what the read-only flag needs: the flag
# counts a default among an option's definitions, so an option with
# both a default and one definition already stops the build.
#
# For a home-manager user another module declares, outside the
# "dotfiles.users" registry, nothing defines the option, and it reads
# as its type's empty value: no host name and six empty lists.
#
# The module system collects imported modules breadth-first, and where
# two modules declare one option, the deeper module's declaration
# decides each attribute both of them state, the "readOnly" flag among
# them. A module that declared the option again with that flag cleared
# would therefore have to sit at least as deep as this one. The
# "modules/home/default.nix" file publishes this module as the
# "flake.modules.homeManager.provisionedHostOption" entry, inside an
# "imports" list of its own, just as the "default" entry beside it
# keeps the "modules/_activation.nix" file inside its own "imports"
# list. The two therefore sit at one depth.
{lib, ...}: {
  options.dotfiles.host = lib.mkOption {
    type = import ./lib/_host-submodule.nix {inherit lib;};
    readOnly = true;
    description = ''
      The selections and exclusions in force for this managed user,
      together with what the machine forbids machine-wide: the
      machine's own record, layered with this user's. The propagation
      module in the "modules/lib/_constructors.nix" file performs that
      layering, so the machine provisions every user it manages and
      each user adds to that. That module is the option's only author
      here; write this user's own selections and exclusions under the
      "dotfiles.users.<name>" record instead.
    '';
  };
}
