{ lib, inputs, ... }:

{
  perSystem =
    { system, ... }:
    {
      checks =
        let
          homeConfigurations = lib.filterAttrs (_: home: home.pkgs.system == system) (
            inputs.self.homeConfigurations or { }
          );
          homeChecks = lib.mapAttrs' (
            name: home: lib.nameValuePair "home-manager-${name}" home.activationPackage
          ) homeConfigurations;

          # darwinConfigurations = lib.filterAttrs (name: darwin: darwin.hostPlatform == system) (
          #   inputs.self.darwinConfigurations or { }
          # );
          # darwinChecks = lib.mapAttrs' (
          #   name: darwin: lib.nameValuePair "nix-darwin-${name}" darwin.config.system.build.toplevel
          # ) darwinConfigurations;
        in
        homeChecks; # // darwinChecks;
    };
}
