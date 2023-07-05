{ config, lib, pkgs, ... }:

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
      #package = lib.mkDefault (if isDarwin then pkgs.emacs-macport else pkgs.emacs29);
      package = lib.mkDefault pkgs.emacs29;
      # TODO(seh): Should we add anything here?
      #extraConfig = ''
      #'';
      extraPackages = epkgs:
        with epkgs; [
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
          dired-subtree
          elvish-mode
          envrc
          epkgs."ido-completing-read+"
          exec-path-from-shell
          flycheck
          flycheck-rust
          flycheck-tip
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
          nix-mode
          nix-modeline
          nixpkgs-fmt
          org
          persistent-scratch
          rust-mode
          slime
          slime-company
          slime-repl-ansi-color
          smex
          terraform-mode
          tide
          use-package
          yaml-mode
          yasnippet
        ];
    };

    services.emacs = {
      # NB: This service is not implemented for macOS as of now.
      enable = !isDarwin;
      defaultEditor = true;
    };
  };
}
