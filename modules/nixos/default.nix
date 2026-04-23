{
  config,
  lib,
  ...
} @ flake: {
  flake.nixosModules.default = {
    imports =
      (lib.attrValues (config.flake.featureModules.nixOS or {}))
      ++ (lib.attrValues (config.flake.profileModules.nixOS or {}))
      ++ [
        ../_tags.nix
        ../_assertions.nix
        {
          dotfiles._knownTags = flake.config.flake.knownTags;
        }
        (
          {
            config,
            lib,
            ...
          }: {
            options.dotfiles = {
              _flakeOptions = lib.mkOption {
                type = with lib.types; uniq (lazyAttrsOf anything);
                default = flake.config.dotfiles;
                internal = true;
                description = ''
                  A writable proxy for the read-only {option}`dotfiles.flakeOptions`
                  option. It's present to let downstream flakes set the option through
                  `lib.mkNixOS`.
                '';
              };

              flakeOptions = lib.mkOption {
                type = with lib.types; lazyAttrsOf anything;
                default = config.dotfiles._flakeOptions;
                readOnly = true;
                description = ''
                  The flake-parts module options set for the dotfiles flake.
                '';
              };
            };

            config = {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                sharedModules =
                  flake.config.dotfiles.home.modules
                  ++ [
                    flake.config.flake.homeModules.default
                    {dotfiles._flakeOptions = config.dotfiles._flakeOptions;}
                  ];
              };
            };
          }
        )
      ];
  };
}
