{
  dotfiles.profiles = {
    enableAll = true;

    development = {
      enableRust = true;
    };
  };
  home.stateVersion = "23.11";
  programs.home-manager.enable = true;
}
