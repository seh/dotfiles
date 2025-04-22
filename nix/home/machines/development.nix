{
  dotfiles.profiles = {
    enableAll = true;

    development = {
      enableRust = true;
    };
  };
  programs.home-manager.enable = true;
}
