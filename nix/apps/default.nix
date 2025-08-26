{
  lib,
  ...
}:

{
  perSystem =
    {
      pkgs,
      inputs',
      self',
      ...
    }:
    {
      apps = {
        home = {
          meta.description = "Home Manager tool";
          program = "${inputs'.home-manager.packages.default}/bin/home-manager";
        };
      }
      // lib.optionalAttrs pkgs.stdenv.isDarwin {
        os = {
          meta.description = "nix-darwin's tool to build and activate configurations";
          program = "${inputs'.nix-darwin.packages.default}/bin/darwin-rebuild";
        };
      }
      // lib.optionalAttrs pkgs.stdenv.isLinux {
        os = {
          meta.description = "NixOS's tool to build and activate configurations";
          program = "${self'.packages.nixos-rebuild}/bin/nixos-rebuild";
        };
      };
    };
}
