# Basis of inspiration:
#   https://github.com/sebastiant/dotfiles/blob/fd3f32073bce885027f7069d870ba4ea254fc348/programs/zsh/zsh.nix
{ config, lib, pkgs, ... }:

let
  cfg = config.dotfiles.zsh;
in
{
  options.dotfiles.zsh = {
    enable = lib.mkEnableOption "zsh";

    enablePowerlevel10k = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to install and activate the Powerlevel10k theme for zsh.";
    };
  };

  config = lib.mkIf cfg.enable {
    # NB: We reference this file from the "zshrc" file.
    home.file.".p10k.zsh" = lib.mkIf cfg.enablePowerlevel10k {
      source = ./p10k.zsh;
    };

    programs.zsh = {
      enable = true;
      autocd = false;
      defaultKeymap = "emacs";
      syntaxHighlighting = {
        enable = true;
      };
      history = {
        expireDuplicatesFirst = true;
        extended = true;
        size = 20000; # Default is 10,000.
      };
      initExtraFirst = lib.mkIf cfg.enablePowerlevel10k
        ''
          # See https://github.com/romkatv/powerlevel10k#how-do-i-initialize-direnv-when-using-instant-prompt.
          (( ''${+commands[direnv]} )) && emulate zsh -c "''$(direnv export zsh)"

          if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''$A''${(%):-%n}.zsh" ]]; then
            source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
          fi

          (( ''${+commands[direnv]} )) && emulate zsh -c "''$(direnv hook zsh)"
        '';
      initExtraBeforeCompInit =
        ''
          zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
          zstyle ':completion:*' subst-globs-only true
          zstyle ':completion:*' list-colors ''${(s.:.)LS_COLORS}
          zstyle ':completion:*' matcher-list ''' 'm:{[:lower:]}={[:upper:]}' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'r:|[._-]=* r:|=*'
          zstyle ':completion:*' menu select=1
          zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
          # As suggested by the Bazel project: https://docs.bazel.build/versions/master/completion.html#zsh.
          zstyle ':completion:*' use-cache on
          zstyle ':completion:*' cache-path ~/.zsh/cache
          zstyle :compinstall filename ~/.zshrc
        '';
      initExtra = builtins.readFile ./zshrc;
      shellAliases = {
        ls = "ls -G";
      };

      antidote = {
        enable = true;
        plugins = [
          # TODO(seh): Confirm that these work when specified directly as oh-my-zsh plugins.
          # "ohmyzsh/ohmyzsh path:lib"
          # "ohmyzsh/ohmyzsh path:plugins/colored-man-pages"
          # "ohmyzsh/ohmyzsh path:plugins/extract"
          # "ohmyzsh/ohmyzsh path:plugins/git"
          "zsh-users/zsh-autosuggestions"
          "zsh-users/zsh-completions"
        ]
        ++ lib.optional cfg.enablePowerlevel10k "romkatv/powerlevel10k"
        ++ [
          # NB: This one needs to come last.
          "zsh-users/zsh-syntax-highlighting"
        ];
        useFriendlyNames = true;
      };
      oh-my-zsh = {
        enable = true;
        plugins = [
          "colored-man-pages"
          "direnv"
          "extract"
        ];
      };
    };
  };
}
