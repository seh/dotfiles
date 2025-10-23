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
        epkgs: with epkgs; [
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
          cue-mode
          dap-mode
          dired-subtree
          doom-modeline
          doom-themes
          edit-indirect
          envrc
          epkgs."ido-completing-read+"
          exec-path-from-shell
          flycheck
          flycheck-rust
          flycheck-tip
          git-modes
          go-mode
          gruvbox-theme
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
          nushell-ts-mode
          org
          org-edit-indirect
          org-grep
          org-modern
          org-roam
          org-roam-timestamps
          persistent-scratch
          prettier
          protobuf-mode
          rg
          rust-mode
          rustic
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
          yaml-pro
          yasnippet
        ];
    };

    services.emacs = {
      # NB: Though this service is implemented for macOS, it is
      # difficult to get it to both create and reuse frames.
      enable = !isDarwin;
      defaultEditor = true;
    };
  };
}
