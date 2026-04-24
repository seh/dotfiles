# Reify a handful of this flake's flake-parts modules under
# "flake.flakeModules" so consumers can opt into them from their own
# evaluators.
#
# "flakeModules.checks" and "flakeModules.style" are consumer-facing
# extensions: a consumer importing either one installs the
# corresponding "perSystem" contributions into its own evaluator.
#
# There is deliberately no "flakeModules.default": the constructors
# ("lib.mkHome", "lib.mkDarwin", "lib.mkNixOS") are reached as plain
# library functions through "inputs.dotfiles.lib.*", closing over
# this flake's own "self" without installing anything into the
# consumer evaluator. All option schemas and overlays relevant to a
# consumer flow in through the modules passed to those constructors,
# inside the target class evaluator where they are actually read.
#
# These outputs flow through flake-parts' raw "flake" passthrough
# rather than through "inputs.flake-parts.flakeModules.flakeModules":
# importing that module declares an alias "flake.flakeModule"
# (singular) for "flake.flakeModules.default", which "nix flake
# check" eagerly evaluates and would abort against because no
# "default" entry exists.
_: {
  flake.flakeModules = {
    checks = ./checks.nix;
    style = ./style.nix;
  };
}
