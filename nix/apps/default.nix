{
  lib,
  self,
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
      apps =
        {
          home.program = "${inputs'.home-manager.packages.default}/bin/home-manager";
        }
        // lib.optionalAttrs pkgs.stdenv.isDarwin {
          os.program = "${inputs'.nix-darwin.packages.default}/bin/darwin-rebuild";
        }
        // lib.optionalAttrs pkgs.stdenv.isLinux {
          os.program = "${self'.packages.nixos-rebuild}/bin/nixos-rebuild";
        };
    };
}
