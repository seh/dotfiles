# Basis of inspiration:
#   https://github.com/midchildan/dotfiles/blob/main/nix/packages/default.nix
{ pkgs ? import <nixpkgs> { }
, nixos ? import <nixos> { }
, inputs ? { }
}:

let
  inherit (pkgs.lib) optionalAttrs;
  inherit (pkgs.stdenv) isDarwin;
  nix = pkgs.nixVersions.${inputs.self.lib.config.nix.package};
in
{
  # TODO(seh): Define common attributes.
} // optionalAttrs isDarwin {
  nix-darwin = pkgs.callPackage ./nix-darwin.nix {
    inherit nix;
    inherit (inputs) nix-darwin;
  };
}
