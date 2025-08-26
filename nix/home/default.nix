{
  inputs,
  ...
}@flake:

let
  dotfiles = inputs.self;
in
{
  imports = [
    ./machines
  ];

  flake.homeModules.default =
    {
      config,
      lib,
      ...
    }:
    {
      imports = dotfiles.lib.importDirs [
        ./modules
        ./profiles
      ];

      options.dotfiles = {
        _flakeOptions = lib.mkOption {
          type = with lib.types; uniq (lazyAttrsOf anything);
          default = flake.config.dotfiles;
          internal = true;
          description = ''
            A writable proxy for the read-only {option}`dotfiles.flakeOptions`
            option. It's present to let downstream flakes set the option through
            `lib.mkHome`.
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
        # NB: This passes this flake as input to these modules via the
        # "dotfiles" attribute.
        _module.args = {
          inherit dotfiles;
        };

        # TODO(seh): Define "nix.registry"?
        # TODO(seh): Define "nix.channels"?
      };
    };
}
