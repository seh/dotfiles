{
  config,
  lib,
  ...
} @ flake: {
  # TODO(seh): Define "nix.registry"?
  # TODO(seh): Define "nix.channels"?
  flake.modules.homeManager.default = {
    imports =
      (lib.attrValues (config.dotfiles.featureModules.homeManager or {}))
      ++ (lib.attrValues (config.dotfiles.profileModules.homeManager or {}))
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
