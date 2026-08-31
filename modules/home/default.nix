{
  config,
  lib,
  ...
} @ flake: {
  # TODO(seh): Define "nix.registry"?
  # TODO(seh): Define "nix.channels"?
  flake.modules.homeManager = {
    default = {
      imports =
        (lib.attrValues (config.dotfiles.featureModules.homeManager or {}))
        ++ [
          ../_activation.nix
          ../_assertions.nix
          ../_user-identity.nix
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

    # The writable declaration of the "dotfiles.host" option, for a
    # home configuration whose own person writes that record. The
    # "mkHome" constructor in the "modules/lib/_constructors.nix" file
    # appends this module beside the "default" one above.
    #
    # The declaration sits inside an "imports" list here, as the
    # "modules/_activation.nix" file does inside the "default" module
    # above, so the two sit at one depth in the import tree. That
    # matters because the module system, where two modules declare one
    # option, takes the deeper declaration's answer for each attribute
    # both of them state.
    hostOption = {imports = [../_host-option.nix];};

    # The read-only declaration of the same option, for a home-manager
    # evaluator that a system configuration builds. The "mkDarwin" and
    # "mkNixOS" constructors give this module to every such evaluator
    # through the "home-manager.sharedModules" option, in place of the
    # writable declaration above.
    provisionedHostOption = {imports = [../_provisioned-host-option.nix];};
  };
}
