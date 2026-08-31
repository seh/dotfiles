{
  config,
  lib,
  ...
} @ flake: {
  flake.modules.nixos.default = {
    imports =
      (lib.attrValues (config.dotfiles.featureModules.nixOS or {}))
      ++ [
        ../_activation.nix
        ../_assertions.nix
        ../_host-option.nix
        ../_host-users.nix
        ../_nixos-user-accounts.nix
        {
          dotfiles = {
            _knownFeatures = flake.config.dotfiles.knownFeatures;
            _knownInterests = flake.config.dotfiles.knownInterests;
            _featureClasses = flake.config.dotfiles.featureClasses;
            _featurePreconditions = flake.config.dotfiles.featurePreconditions;
            _impliedEdges = flake.config.dotfiles.impliedEdges;
            _supportedPlatforms = flake.config.dotfiles.supportedPlatforms;
            _flakeLib = flake.config.flake.lib;
          };
        }
      ];
  };
}
