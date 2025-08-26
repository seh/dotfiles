_:

{
  perSystem = _: {
    dotfiles.callPackages = {
      # TODO(seh): Enable this once we have at least one package
      # defined in the subdirectory.
      enable = false;
      directory = ./by-name;
    };
  };
}
