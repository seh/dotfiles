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
        ./module-schema.nix
        ./config.nix
        ./nixpkgs-config.nix
        ./lib/default.nix
        ./lib/flake-module.nix
        ./nix-darwin/default.nix
        (inputs.import-tree ./nix-darwin/features)
        (inputs.import-tree ./nix-darwin/profiles)
        ./home/default.nix
        (inputs.import-tree ./home/features)
        (inputs.import-tree ./home/profiles)
        ./nixos/default.nix
        (inputs.import-tree ./nixos/features)
        ./overlays/default.nix
        ./packages/flake-modules.nix
      ];
    };
    checks = ./checks.nix;
    style = ./style.nix;
  };
}
