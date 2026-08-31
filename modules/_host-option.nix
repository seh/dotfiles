# The writable declaration of the "dotfiles.host" option, for an
# evaluator whose own author writes that option. The nix-darwin and
# NixOS class aggregators import this module, so a machine's
# configuration writes the option. The "mkHome" constructor in the
# "modules/lib/_constructors.nix" file appends the
# "flake.modules.homeManager.hostOption" entry that imports it, so the
# one person a home configuration serves directly writes the option
# too.
#
# A managed user's nested home-manager evaluator never receives this
# module. It receives the read-only declaration in the
# "modules/_provisioned-host-option.nix" file instead, so the module
# system stops the build on a definition of the option written inside
# that user's own configuration.
{lib, ...}: {
  options.dotfiles.host = lib.mkOption {
    type = import ./lib/_host-submodule.nix {inherit lib;};
    default = {};
    description = ''
      The host record this evaluator acts on: the names it selects
      under its "features" and "interests" lists, the exclusions it
      applies to its own walk, and the machine-wide
      "forbidFeatures"/"forbidInterests" lists. In a system evaluator
      these fields state the machine's own wants alone, standing
      alongside the users in the "dotfiles.users" registry and taking
      nothing from them, so no user's selection configures the
      machine. On a host that home-manager alone manages, the sole
      user is the machine, so these are that user's selections. A
      consumer's own module may extend these lists through the module
      system's append-merge. A managed user's nested home-manager
      evaluator receives the read-only declaration in the
      "modules/_provisioned-host-option.nix" file instead of this one.
    '';
  };
}
