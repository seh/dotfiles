{
  config,
  lib,
  ...
} @ flake: {
  flake.modules.darwin.default = {
    imports =
      (lib.attrValues (config.dotfiles.featureModules.nixDarwin or {}))
      ++ (lib.attrValues (config.dotfiles.profileModules.nixDarwin or {}))
      ++ [
        ../_activation.nix
        ../_assertions.nix
        ../_host-users.nix
        ../_darwin-primary-user.nix
        ./_reload-launch-agents.nix
        {
          dotfiles = {
            _knownProfiles = flake.config.dotfiles.knownProfiles;
            _knownFeatures = flake.config.dotfiles.knownFeatures;
            _knownInterests = flake.config.dotfiles.knownInterests;
            _featureClasses = flake.config.dotfiles.featureClasses;
            _featurePreconditions = flake.config.dotfiles.featurePreconditions;
            _impliedEdges = flake.config.dotfiles.impliedEdges;
            _profileClasses = flake.config.dotfiles.profileClasses;
            _supportedPlatforms = flake.config.dotfiles.supportedPlatforms;
            _flakeLib = flake.config.flake.lib;
          };
        }
      ];
  };
}
