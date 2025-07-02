{
  flake-parts-lib,
  inputs,
  lib,
  ...
}:

let
  inherit (flake-parts-lib) importApply;

  flakeModules = {
    default = {
      imports = [
        (importApply ./config.nix { inherit inputs; })
        (importApply ./lib/flake-module.nix { inherit inputs; })
        (importApply ./packages/flake-modules.nix { inherit inputs; })
      ];
    };
    checks = ./checks.nix;
    style = ./style.nix;
  };
in
{
  imports = lib.attrValues flakeModules ++ [
    inputs.flake-parts.flakeModules.flakeModules
    ./apps
    ./home
    ./lib
    ./nix-darwin
    ./overlays
    ./packages
    ./templates
  ];

  flake = {
    inherit flakeModules;
  };
}
