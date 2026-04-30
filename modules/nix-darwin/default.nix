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
        ../_tags.nix
        ../_assertions.nix
        ../_user-identity.nix
        {
          dotfiles = {
            _knownProfiles = flake.config.dotfiles.knownProfiles;
            _knownFeatures = flake.config.dotfiles.knownFeatures;
            _flakeLib = flake.config.flake.lib;
          };
        }
      ];
  };
}
