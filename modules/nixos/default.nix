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
        ../_host-users.nix
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
