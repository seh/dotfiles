{
  inputs,
  lib,
  ...
}: let
  nixpkgsDefaults = import ./_nixpkgs-defaults.nix;
in {
  perSystem = {system, ...}: {
    _module.args.pkgs = lib.mkForce (import inputs.nixpkgs ({inherit system;} // nixpkgsDefaults));
  };
}
