# Publish "flake.lib" as this flake's library namespace. The
# actual aggregation lives in "./_assembly.nix" so that
# "../../flake.nix" can import the same value and thread it
# through "mkFlake"'s "specialArgs".
{
  inputs,
  lib,
  ...
}: {
  config.flake.lib = import ./_assembly.nix {inherit inputs lib;};
}
