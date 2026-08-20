# Basis of inspiration:
#   https://github.com/sebastiant/dotfiles/blob/fd3f32073bce885027f7069d870ba4ea254fc348/programs/zsh/zsh.nix
{flakeLib, ...}:
flakeLib.mkFeature "shell/zsh" {
  homeManager = {
    options = {lib, ...}: {
      options.dotfiles.shell.zsh = {
        enablePowerlevel10k = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = ''
            Whether to install and activate the Powerlevel10k theme
            for zsh.
          '';
        };
      };
    };

    config = {
      config,
      lib,
      pkgs,
      ...
    }: let
      cfg = config.dotfiles.shell.zsh;
    in {
      # NB: We reference this file from the "zshrc" file.
      home.file.".p10k.zsh" = lib.mkIf cfg.enablePowerlevel10k {source = ./p10k.zsh;};

      # The "zsh-completions" package provides no plugin file to
      # source; it instead installs completion functions into its
      # "share/zsh/site-functions" directory. Adding the package to
      # the installed set here places that directory onto zsh's
      # "fpath" search path by way of the Home Manager profile, which
      # the generated "zshenv" file scans for each entry in the
      # "NIX_PROFILES" variable, so that the "compinit" function finds
      # those completions.
      home.packages = [pkgs.zsh-completions];

      programs.zsh = {
        autocd = false;
        defaultKeymap = "emacs";
        enable = true;
        history = {
          expireDuplicatesFirst = true;
          extended = true;
          size = 20000; # Default is 10,000.
        };
        initContent = lib.mkMerge [
          (lib.mkIf cfg.enablePowerlevel10k (
            lib.mkBefore ''
              # See https://github.com/romkatv/powerlevel10k#how-do-i-initialize-direnv-when-using-instant-prompt.
              (( ''${+commands[direnv]} )) && emulate zsh -c "''$(direnv export zsh)"

              if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''$A''${(%):-%n}.zsh" ]]; then
                source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
              fi

              (( ''${+commands[direnv]} )) && emulate zsh -c "''$(direnv hook zsh)"
            ''
          ))
          (lib.mkOrder 550 ''
            zstyle ':completion:*' cache-path ~/.zsh/cache
            zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
            # As suggested by the carpace project: https://carapace-sh.github.io/carapace-bin/setup.html#zsh
            # See also:
            # https://zsh.sourceforge.io/Guide/zshguide06.html#l158
            zstyle ':completion:*' format $'\e[2;37mCompleting %d\e[m'
            zstyle ':completion:*' group-name '''
            zstyle ':completion:*:-command-' group-order builtins functions commands
            zstyle ':completion:*' list-colors ''${(s.:.)LS_COLORS}
            zstyle ':completion:*' matcher-list ''' 'm:{[:lower:]}={[:upper:]}' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'r:|[._-]=* r:|=*'
            zstyle ':completion:*' menu select=1
            zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
            zstyle ':completion:*' subst-globs-only true
            # As suggested by the Bazel project: https://docs.bazel.build/versions/master/completion.html#zsh.
            zstyle ':completion:*' use-cache on
            zstyle :compinstall filename ~/.zshrc
          '')
          (builtins.readFile ./zshrc)
        ];
        oh-my-zsh = {
          enable = true;
          plugins = [
            "colored-man-pages"
            "direnv"
            "extract"
          ];
        };
        plugins =
          [
            {
              name = "zsh-autosuggestions";
              src = pkgs.zsh-autosuggestions;
              file = "share/zsh-autosuggestions/zsh-autosuggestions.zsh";
            }
          ]
          ++ lib.optional cfg.enablePowerlevel10k {
            name = "powerlevel10k";
            src = pkgs.zsh-powerlevel10k;
            file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
          };
        shellAliases = {
          ls = "ls --color=auto --hyperlink=auto";
        };
        siteFunctions = {
          kuc = flakeLib.onlyWhen config ["kubernetes"] (builtins.readFile ./kuc);
        };
        syntaxHighlighting = {
          enable = true;
        };
      };
    };
  };
}
