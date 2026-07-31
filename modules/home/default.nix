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
        ../_activation.nix
        ../_assertions.nix
        ../_user-identity.nix
        # The "dotfiles.users" registry is declared in every class
        # (see "modules/_users.nix"), but only a system
        # configuration acts on its entries. In the home-manager
        # class an entry would exist and do nothing. Such an idle
        # attribute is an attractive nuisance, so demand that the
        # registry stay empty here.
        ({
          config,
          lib,
          ...
        }: let
          userNames = builtins.attrNames config.dotfiles.users;
        in {
          assertions = [
            {
              assertion = userNames == [];
              message = ''
                Resolving host "${toString config.dotfiles.host.name}": "dotfiles.users" names the user(s) ${lib.concatMapStringsSep ", " (n: "\"${n}\"") userNames}, but managed users exist only on hosts that a system configuration manages (nix-darwin or NixOS); a standalone home-manager configuration is one user's environment. Remove the entries.
              '';
            }
          ];
        })
        {
          dotfiles = {
            _knownProfiles = flake.config.dotfiles.knownProfiles;
            _knownFeatures = flake.config.dotfiles.knownFeatures;
            _knownInterests = flake.config.dotfiles.knownInterests;
            _featureClasses = flake.config.dotfiles.featureClasses;
            _featurePreconditions = flake.config.dotfiles.featurePreconditions;
            _impliedEdges = flake.config.dotfiles.impliedEdges;
            _profileClasses = flake.config.dotfiles.profileClasses;
            _profileSupportedPlatforms = flake.config.dotfiles.profileSupportedPlatforms;
            _flakeLib = flake.config.flake.lib;
          };
        }
      ];
  };
}
