{
  lib,
  inputs,
  ...
}: {
  perSystem = {system, ...}: {
    checks = let
      homeConfigurations = lib.filterAttrs (_: home: home.pkgs.stdenv.hostPlatform.system == system) (
        inputs.self.homeConfigurations or {}
      );
      homeChecks =
        lib.mapAttrs' (
          name: home: lib.nameValuePair "home-manager-${name}" home.activationPackage
        )
        homeConfigurations;
    in
      homeChecks;
  };
}
