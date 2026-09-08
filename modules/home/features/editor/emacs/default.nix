{flakeLib, ...}:
flakeLib.mkFeature "editor/emacs" {
  homeManager = {
    config,
    lib,
    pkgs,
    ...
  }: let
    inherit (pkgs.stdenv.hostPlatform) isDarwin;
    inherit (config.dotfiles._host) inEffect;
    quoteAll = names: lib.concatMapStringsSep "\n    " (name: ''"${name}"'') (lib.sort lib.lessThan names);
  in {
    home.file = {
      ".emacs.d" = {
        source = ./.emacs.d;
        recursive = true;
      };
      ".emacs.d/activation-names.el".text = ''
        ;;; -*- lexical-binding: t -*-
        ;; The dotfiles flake writes this file when it builds this home
        ;; environment; do not edit it by hand.

        (defconst seh-known-activation-names
          '(${quoteAll config.dotfiles._knownNames})
          "Every activation name the flake's \"mkFeature\" and \"mkInterest\"
        functions register. An activation name is the string a person writes
        into a host or user record to select a feature or interest.")

        (defconst seh-activation-names-in-effect
          '(${quoteAll (config.dotfiles._host.activeFeatures ++ config.dotfiles._host.expressedInterests)})
          "The activation names in effect for this home environment.
        The dotfiles flake writes this file for the home environment it
        builds; another program may write it instead.")
      '';
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
      extraPackages = epkgs:
        with epkgs;
          [
            abyss-theme
            auctex
            bbdb
            beacon
            boxquote
            color-theme-modern
            color-theme-sanityinc-solarized
            color-theme-sanityinc-tomorrow
            company
            company-auctex
            counsel
            csv-mode
            dired-subtree
            doom-modeline
            doom-themes
            edit-indirect
            envrc
            exec-path-from-shell
            flycheck
            flycheck-tip
            gruvbox-theme
            haskell-mode
            helpful
            iedit
            inf-ruby
            ivy
            ivy-prescient
            jq-ts-mode
            js2-mode
            json-mode
            monokai-theme
            nerd-icons
            nix-mode
            nix-modeline
            org
            org-edit-indirect
            org-grep
            org-modern
            org-roam
            org-roam-timestamps
            ox-typst
            persistent-scratch
            prescient
            rg
            smex
            swiper
            templ-ts-mode
            # NB: THis is a temporary concession until Emacs 30 makes it
            # easier to accommodate treesitter.
            treesit-auto
            treesit-grammars.with-all-grammars
            typst-ts-mode
            use-package
            yaml-mode
            yaml-pro
            yasnippet
          ]
          ++ lib.optionals (inEffect "cloud/terraform") [
            terraform-mode
          ]
          ++ lib.optionals (inEffect "dev/bazel") [
            bazel
          ]
          ++ lib.optionals (inEffect "dev/difftastic") [
            difftastic
          ]
          ++ lib.optionals (inEffect "dev/language-servers") [
            dap-mode
            lsp-ivy
            lsp-mode
            lsp-ui
          ]
          ++ lib.optionals (inEffect "lang/common-lisp") [
            slime
            slime-company
            slime-repl-ansi-color
          ]
          ++ lib.optionals (inEffect "lang/cue") [
            cue-mode
          ]
          ++ lib.optionals (inEffect "lang/go") [
            go-mode
          ]
          ++ lib.optionals (inEffect "lang/javascript/tools") [
            prettier
          ]
          ++ lib.optionals (inEffect "lang/jsonnet") [
            jsonnet-mode
          ]
          ++ lib.optionals (inEffect "lang/lua") [
            lua-mode
          ]
          ++ lib.optionals (inEffect "lang/markdown") [
            markdown-mode
          ]
          ++ lib.optionals (inEffect "lang/protobuf") [
            protobuf-mode
          ]
          ++ lib.optionals (inEffect "lang/rust") [
            cargo # NB: There is also cargo-mode, which is different.
            cargo-transient
            flycheck-rust
            rust-mode
            rustic
          ]
          ++ lib.optionals (lib.any inEffect [
            "model-agent/claude"
            "model-agent/copilot"
            "model-agent/opencode"
          ]) [
            agent-shell
          ]
          ++ lib.optionals (inEffect "shell/nushell") [
            nushell-ts-mode
          ]
          ++ lib.optionals (inEffect "vcs/git") [
            git-modes
            magit
            magit-diff-flycheck
            magit-filenotify
            magit-todos
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
