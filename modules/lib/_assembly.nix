# Plain-data assembly of this flake's library namespace. Imported
# by both:
#
#   - "./default.nix", which assigns the result to "flake.lib" so
#     downstream consumers can reach it via "inputs.dotfiles.lib.*".
#
#   - "../../flake.nix", which threads the result through
#     "mkFlake"'s "specialArgs" as the "flakeLib" module argument
#     so that feature and profile files in this flake's tree can
#     take "{flakeLib, ...}" without triggering the "_module.args"
#     recursion that occurs when such arguments are computed
#     inside a "config" block.
#
# The leading underscore in the filename excludes this file from
# "import-tree" in "../../flake.nix"; it is imported explicitly by
# both call sites.
{
  inputs,
  lib,
}: let
  cascades = import ./_cascades.nix {inherit lib;};
  constructors = import ./_constructors.nix {inherit lib inputs;};
  features = import ./_features.nix {inherit lib;};
in
  cascades // constructors // features
