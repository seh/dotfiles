{flakeLib, ...}:
flakeLib.mkFeature "editor/emacs" {
  homeManager = {
    options = {lib, ...}: {
      options.dotfiles.editor.emacs.failOnCompileWarnings = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = ''
          Whether a byte-compiler warning in the Emacs Lisp files fails the
          build of the ".emacs.d" directory. Set this to false to build
          despite a warning, such as one a package update introduces, until
          the file is corrected.
        '';
      };
    };
    config = {
      config,
      lib,
      pkgs,
      ...
    }: let
      inherit (pkgs.stdenv.hostPlatform) isDarwin;
      inherit (config.dotfiles._host) inEffect;
      quoteAll = names: lib.concatMapStringsSep "\n    " (name: ''"${name}"'') (lib.sort lib.lessThan names);

      # The activation names the flake registers and the ones in effect
      # for this home environment, as an Emacs Lisp file that the
      # "activation.el" file consults.
      activationNames = pkgs.writeText "activation-names.el" ''
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

      # Byte-compile the Emacs Lisp files with the Emacs that Home Manager
      # installs, so every package is on the compiler's load path.
      emacsDirectory =
        pkgs.runCommand "emacs.d" {
          nativeBuildInputs = [config.programs.emacs.finalPackage];
        } ''
          cp --recursive ${./.emacs.d} "$out"
          chmod --recursive u+w "$out"
          cp ${activationNames} "$out/activation-names.el"
          cd "$out"
          # Compile each file in its own Emacs process, so definitions one
          # file leaves loaded cannot hide warnings in the next.
          for file in ./*.el; do
            emacs --batch \
              --load "$out/activation-names.el" \
              --load "$out/activation.el" \
              --eval '(setq byte-compile-error-on-warn ${
            if config.dotfiles.editor.emacs.failOnCompileWarnings
            then "t"
            else "nil"
          })' \
              --funcall batch-byte-compile "$file"
          done
        '';
    in {
      home.file.".emacs.d" = {
        source = emacsDirectory;
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
        extraPackages = epkgs:
          with epkgs;
            [
              abyss-theme
              auctex
              bbdb
              beacon
              # An autoload cookie above a macro call copies the call into
              # the generated autoloads file, where Emacs cannot expand it
              # yet; the patch autoloads the command explicitly instead.
              (boxquote.overrideAttrs (previous: {
                patches = (previous.patches or []) ++ [./patches/boxquote-autoload.patch];
              }))
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
              # As with "boxquote" above. The ELPA builder installs the
              # tarball as is, so the patch applies to a copy of its contents,
              # packed again under the same name.
              (typst-ts-mode.overrideAttrs (previous: {
                src = pkgs.runCommand previous.src.name {} ''
                  tar --extract --file ${previous.src}
                  patch --directory "${previous.pname}-${previous.version}" --strip=1 \
                    < ${./patches/typst-ts-mode-autoload.patch}
                  tar --create --file "$out" "${previous.pname}-${previous.version}"
                '';
              }))
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
            ++ lib.optionals (inEffect "lang/zig") [
              zig-ts-mode
            ]
            ++ lib.optionals (inEffect "lang/zig" && !(inEffect "lang/zig/ls")) [
              reformatter
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
  };
}
