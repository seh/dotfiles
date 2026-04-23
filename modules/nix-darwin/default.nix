{inputs, ...} @ flake: {
  flake.darwinModules.default = {
    config,
    lib,
    ...
  }: {
    imports = [
      (inputs.import-tree ./features)
      ../_tags.nix
      ../_assertions.nix
    ];

    options.dotfiles = {
      _flakeOptions = lib.mkOption {
        type = with lib.types; uniq (lazyAttrsOf anything);
        default = flake.config.dotfiles;
        internal = true;
        description = ''
          A writable proxy for the read-only {option}`dotfiles.flakeOptions`
          option. It's present to let downstream flakes set the option through
          `lib.mkDarwin`.
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
  };
}
