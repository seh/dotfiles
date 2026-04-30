{
  config,
  lib,
  ...
} @ flake: {
  flake.modules.nixos.default = {
    imports =
      (lib.attrValues (config.dotfiles.featureModules.nixOS or {}))
      ++ (lib.attrValues (config.dotfiles.profileModules.nixOS or {}))
      ++ [
        ../_tags.nix
        ../_assertions.nix
        ../_user-identity.nix
        {
          dotfiles = {
            _knownProfiles = flake.config.dotfiles.knownProfiles;
            _knownFeatures = flake.config.dotfiles.knownFeatures;
            _flakeLib = flake.config.flake.lib;
          };
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            sharedModules = [
              flake.config.flake.modules.homeManager.default
            ];
          };
        }
      ];
  };
}
