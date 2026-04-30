# Reify a handful of this flake's flake-parts modules under
# "flake.modules.flake" so consumers can opt into them from their
# own evaluators.
#
# "flake.modules.flake.checks" and "flake.modules.flake.style" are
# consumer-facing extensions: a consumer importing either one
# installs the corresponding "perSystem" contributions into its own
# evaluator.
#
# There is deliberately no "flake.modules.flake.default": the
# constructors ("lib.mkHome", "lib.mkDarwin", "lib.mkNixOS") are
# reached as plain library functions through
# "inputs.dotfiles.lib.*", closing over this flake's own "self"
# without installing anything into the consumer evaluator. All
# option schemas and overlays relevant to a consumer flow in
# through the modules passed to those constructors, inside the
# target class evaluator where they are actually read.
#
# Importing "inputs.flake-parts.flakeModules.modules" pulls in the
# class-aware "flake.modules.<class>.<name>" option schema, which
# enforces module-class checking and supersedes the legacy flat
# "flake.{home,darwin,nixos,flake}Modules" passthroughs.
{inputs, ...}: {
  imports = [
    inputs.flake-parts.flakeModules.modules
  ];
  flake.modules.flake = {
    checks = ./checks.nix;
    style = ./style.nix;
  };
}
