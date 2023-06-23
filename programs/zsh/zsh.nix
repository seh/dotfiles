# Basis of inspiration:
#   https://github.com/sebastiant/dotfiles/blob/fd3f32073bce885027f7069d870ba4ea254fc348/programs/zsh/zsh.nix
{ config, lib, pkgs, ... }:
{
  programs.zsh = {
    enable = true;
    defaultKeymap = "emacs";
    enableSyntaxHighlighting = true;
    history = {
      expireDuplicatesFirst = true;
      extended = true;
    };
    # TODO(seh): Continue here.

    antidote = {
      enable = true;
      plugins = [
        # TODO(seh): Add more here.        
      ];
      useFriendlyNames = true;
    };
    oh-my-zsh = {
      enable = true;
      plugins = [
        # TODO(seh): Add more here.
        "direnv"
        "git"
        "nix-shell"
      ];
    };
  };
  programs.fzf.enableZshIntegration = true;
}
