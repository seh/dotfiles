# Reify the public-API "flake.flakeModules" attribute set so that
# downstream flakes can opt into specific subsets of this flake's
# flake-parts modules (rather than importing everything wholesale).
#
# The modules named below also self-activate in this flake by way of
# the top-level "import-tree" invocation in "./flake.nix"; their
# appearance here is solely to expose them under "flakeModules" for
# external consumers.
{inputs, ...}: {
  imports = [
    inputs.flake-parts.flakeModules.flakeModules
  ];

  flake.flakeModules = {
    default = {
      imports = [
        ./config.nix
        ./nixpkgs-config.nix
        ./lib/flake-module.nix
        ./packages/flake-modules.nix
      ];
    };
    checks = ./checks.nix;
    style = ./style.nix;
  };
}
