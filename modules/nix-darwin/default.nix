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
        ./_reload-launch-agents.nix
        {
          dotfiles = {
            _knownProfiles = flake.config.dotfiles.knownProfiles;
            _knownFeatures = flake.config.dotfiles.knownFeatures;
            _profileSupportedPlatforms = flake.config.dotfiles.profileSupportedPlatforms;
            _flakeLib = flake.config.flake.lib;
          };
        }
      ];
  };
}
