{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  cfg = config.dotfiles.emacs;
in
{
  options.dotfiles.emacs.enable = lib.mkEnableOption "Emacs";

  config = lib.mkIf cfg.enable {
    home.file.".emacs.d" = {
      source = ./.emacs.d;
      recursive = true;
    };

    programs.emacs = {
      enable = lib.mkDefault true;
      # NB: Unfortunately, with the "emacs-macport" package, the
      # C-M-SPC key binding for "mark-sexy" gets intercepted by macOS
      # and presents the Character Viewer applet.
      #package = lib.mkDefault (if isDarwin then pkgs.emacs-macport else pkgs.emacs);
      package = lib.mkDefault pkgs.emacs;
      # TODO(seh): Should we add anything here?
      #extraConfig = ''
      #'';
      extraPackages =
        epkgs:
        with epkgs;
        let
          my-cue-mode = epkgs.trivialBuild {
            pname = "cue-mode";
            version = "0.1";
            src = ./packages/cue-mode.el;
            meta = {
              description = "Major mode for editing CUE files";
              maintainers = [
                {
                  name = "Steven E. Harris";
                  email = "seh@panix.com";
                  github = "seh";
                  githubID = 175841;
                }
              ];
            };
          };
        in
        [
          abyss-theme
          auctex
          bazel
          bbdb
          beacon
          boxquote
          cargo # NB: There is also cargo-mode, which is different.
          cargo-transient
          color-theme-modern
          color-theme-sanityinc-solarized
          color-theme-sanityinc-tomorrow
          company
          company-auctex
          csv-mode
          dired-subtree
          doom-modeline
          doom-themes
          elvish-mode
          envrc
          epkgs."ido-completing-read+"
          exec-path-from-shell
          flycheck
          flycheck-rust
          flycheck-tip
          git-modes
          go-mode
          haskell-mode
          helpful
          inf-ruby
          js2-mode
          json-mode
          jsonnet-mode
          lsp-mode
          lsp-ui
          lua-mode
          magit
          magit-diff-flycheck
          magit-filenotify
          magit-todos
          markdown-mode
          monokai-theme
          nerd-icons
          nix-mode
          nix-modeline
          org
          org-grep
          org-modern
          org-roam
          org-roam-timestamps
          persistent-scratch
          prettier
          protobuf-ts-mode
          rust-mode
          slime
          slime-company
          slime-repl-ansi-color
          smex
          templ-ts-mode
          terraform-mode
          # NB: THis is a temporary concession until Emacs 30 makes it
          # easier to accommodate treesitter.
          treesit-auto
          treesit-grammars.with-all-grammars
          use-package
          yaml-mode
          yasnippet
        ]
        # Local or ad hoc packages
        ++ [ my-cue-mode ];
    };

    services.emacs = {
      # NB: This service is not implemented for macOS as of now.
      enable = !isDarwin;
      defaultEditor = true;
    };
  };
}
