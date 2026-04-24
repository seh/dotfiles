# Publish "flake.lib" as this flake's library namespace. Two
# topically distinct bodies contribute to it: the cascade-graph
# helpers in "./_cascades.nix" and the class-configuration
# constructors in "./_constructors.nix". Both are plain
# attrset-returning files (the leading underscore excludes them
# from "import-tree" in "../../flake.nix") imported here and
# merged into a single assignment, honoring flake-parts'
# uniqueness constraint on "flake.lib".
{
  inputs,
  lib,
  ...
}: let
  cascades = import ./_cascades.nix {inherit lib;};
  constructors = import ./_constructors.nix {inherit lib inputs;};
in {
  config.flake.lib = cascades // constructors;
}
