{
  dotfiles.profiles = {
    enableAll = true;

    development = {
      enableRust = true;
    };
  };
  home.stateVersion = "25.11";
  programs.home-manager.enable = true;
}
