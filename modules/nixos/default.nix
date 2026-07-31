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
        ../_activation.nix
        ../_assertions.nix
        ../_host-users.nix
        {
          dotfiles = {
            _knownProfiles = flake.config.dotfiles.knownProfiles;
            _knownFeatures = flake.config.dotfiles.knownFeatures;
            _knownInterests = flake.config.dotfiles.knownInterests;
            _featureClasses = flake.config.dotfiles.featureClasses;
            _featurePreconditions = flake.config.dotfiles.featurePreconditions;
            _impliedEdges = flake.config.dotfiles.impliedEdges;
            _profileSupportedPlatforms = flake.config.dotfiles.profileSupportedPlatforms;
            _flakeLib = flake.config.flake.lib;
          };
        }
      ];
  };
}
