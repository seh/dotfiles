;;; -*- lexical-binding: t -*-
;:* packages.el
;:*=======================
(declare-function seh-activation-name-in-effect-p "activation")

;; * The `package' system itself
(require 'package)

;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)))
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; Basis of inspiration:
;;   https://github.com/bdd/.emacs.d/blob/master/packages.el

(require 'use-package)


;:*=======================
;:* beacon
(use-package beacon
  :hook (after-init . beacon-mode)
  :config
  (setq
   ;; This is 0.3 by default. Values lower than 0.2 don't appear to
   ;; make a difference.
   beacon-blink-duration 0.2))


;:*=======================
;:* counsel
(use-package counsel
  :functions (counsel-mode)
  :after ivy
  :config
  (counsel-mode))


;:*=======================
;:* difftastic
(use-package difftastic-bindings
  :if (seh-activation-name-in-effect-p "dev/difftastic")
  :ensure difftastic
  :functions (difftastic-bindings-mode)
  :config
  (difftastic-bindings-mode))


;:*=======================
;:* dired
;; TODO: Bind 'K' to dired-kill-subdir

;; This per http://pragmaticemacs.com/emacs/tree-style-directory-views-in-dired-with-dired-subtree/:
(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)))


;:*=======================
;:* diary
(use-package diary-lib
  :defer t
  :defines (diary-file)
  :init
  (setq diary-file "~/.diary"))


;:*=======================
;:* doom-modeline
(use-package nerd-icons)

;; Basis of inspiration: https://config.daviwil.com/emacs#doom-modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 15
        doom-modeline-bar-width 6
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-minor-modes t
        doom-modeline-major-mode-icon nil
        doom-modeline-lsp t))


;:*=======================
;:* envrc
(use-package envrc
  ;; See the following discussion for why we call this as late as feasible:
  ;; https://github.com/purcell/envrc#usage
  :hook (after-init . envrc-global-mode))


;:*=======================
;:* exec-path-from-shell
(use-package exec-path-from-shell
  :if window-system
  :functions (exec-path-from-shell-initialize)
  :config
  (dolist (var '("GOPATH"
                 "XDG_CONFIG_DIRS"
                 ;; Within Nix, hunspel needs this variable to find its dictionaries.
                 "XDG_DATA_DIRS"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))


;:*=======================
;:* footnote
(use-package footnote
  :config
  (setq footnote-body-tag-spacing 1
      footnote-spaced-footnotes nil
      ;; These are for footnote.el version 0.19.
      ;; For version 0.20, use style `numeric-latin' and
      ;; drop use of the start and end tags.
      footnote-style 'latin
      footnote-start-tag ""
      footnote-end-tag "")
  :hook (message-mode . footnote-mode))


;:*=======================
;:* flyspell
(use-package flyspell
  :hook (prog-mode . flyspell-prog-mode))


;:*=======================
;:* gnus
(use-package gnus
  :defer t
  :defines (gnus-directory gnus-kill-files-directory message-directory)
  :functions (turn-on-gnus-dired-mode)
  ;; Unless we use `custom-set-variables' here, we can't wait until
  ;; the package is loaded to set these, as other custom-based variables
  ;; depend upon them.
  :init
  (setq gnus-directory "~/doc/news"
        message-directory gnus-directory)
  :config
  (setq gnus-kill-files-directory gnus-directory)
  (require 'gnus-dired)
  (add-hook 'dired-mode-hook #'turn-on-gnus-dired-mode))
;; the rest of the configuration is in ~/.gnus


;:*=======================
;:* ivy
(use-package ivy
  :functions (ivy-completing-read ivy-immediate-done ivy-mode)
  :config
  ;; This same function is available via "C-M-j" as well, but that's
  ;; harder to type. By default, "M-RET" is bound to "ivy-call", which
  ;; isn't as useful.
  (define-key ivy-minibuffer-map (kbd "M-RET") #'ivy-immediate-done)
  (setq ivy-count-format "[%d/%d] "
        ivy-height 7
        ivy-use-virtual-buffers 'recentf
        ivy-virtual-abbreviate 'abbreviate)
  (ivy-mode))


;:*=======================
;:* ivy-prescient
(use-package ivy-prescient
  :functions (ivy-prescient-mode)
  :after counsel
  :config
  (ivy-prescient-mode))


;:*=======================
;:* magit
(use-package magit
  :if (seh-activation-name-in-effect-p "vcs/git")
  :preface
  (defun seh-git-commit-skip-jujutsu-diff ()
    "Stop Magit from diffing the working tree for a Jujutsu description.
Magit shows a diff of the working tree whenever it recognizes a commit
message; for a Jujutsu description, that diff is Git's view of a
Jujutsu working tree, which need not resemble the change being
described and can be huge."
    (when (and buffer-file-name
               (string-suffix-p ".jjdescription" buffer-file-name))
      (setq-local magit-commit-show-diff nil)))
  :init
  (when (seh-activation-name-in-effect-p "vcs/jujutsu")
    (add-hook 'git-commit-setup-hook #'seh-git-commit-skip-jujutsu-diff))
  :config
  (setq magit-completing-read-function #'ivy-completing-read
        ;; Edit jj commit messages using "git-commit-mode":
        ;; Default value: "/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'"
        git-commit-filename-regexp "/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\|\\.jjdescription\\'"
        git-commit-summary-max-length 50))


;:*=======================
;:* nix
(use-package nix
  :defines (lsp-nix-nixd-formatting-command nix-nixfmt-bin)
  :functions (nix-format-before-save)
  :hook
  (nix-mode . (lambda ()
                (electric-pair-mode)
                (when (seh-activation-name-in-effect-p "essential/tools")
                  (add-hook 'before-save-hook #'nix-format-before-save 0 t))))
  :config
  ;; By default, this formatting program is "nixfmt".
  ;;
  ;; NB: As of this writing, neither the `nix--format-call' function
  ;; nor the `nixd' language server's implementation of
  ;; `lsp-format-buffer' supply any arguments to the programs that
  ;; they invoke. Work around this problem by using a small trampoline
  ;; program to supply the necessary arguments.
  (when (seh-activation-name-in-effect-p "essential/tools")
    (let ((formatter-command "alejandra-quiet"))
      (setq
       nix-nixfmt-bin formatter-command
       lsp-nix-nixd-formatting-command (vector formatter-command)))))


;:*=======================
;:* ox-typst
(use-package ox-typst
  :after org)


;:*=======================
;:* persistent-scratch
(use-package persistent-scratch
  :functions (persistent-scratch-mode persistent-scratch-restore)
  :demand t
  :hook (after-init . (lambda ()
                        (when (file-exists-p persistent-scratch-save-file)
                          (persistent-scratch-restore))
                        (with-current-buffer "*scratch*"
                          (persistent-scratch-mode)))))


;:*=======================
;:* prescient
(use-package prescient
  :functions (prescient-persist-mode)
  :config
  (prescient-persist-mode))


;:*=======================
;:* project
(use-package project
  :defer t
  :config
  (add-to-list 'project-vc-extra-root-markers ".jj"))


;:*=======================
;:* recentf
(use-package recentf
  :hook (buffer-list-update . recentf-track-opened-file))


;:*=======================
;:* rg
(use-package rg
  :functions (rg-enable-menu)
  :config
  (rg-enable-menu))


;:*=======================
;:* sh-mode
(use-package sh-script
  :defer t
  :config
  (setq sh-basic-offset 2))


;:*=======================
;:* shell
(use-package shell
  :functions (flycheck-mode)
  :hook (shell-mode . (lambda ()
                        (setq shell-prompt-pattern "^\\[[^\n]+\\]\n[#$%>] *")
                        (custom-set-faces
                         '(shell-output-face ((((class color) (background dark))
			                       (:italic nil :foreground "gray90"))
			                      (((class color) (background light))
			                       (:italic nil :foreground "darkblue"))) t)

                         '(shell-prompt-face ((((class color) (background dark))
			                       (:bold t :foreground "palegreen"))
			                      (((class color) (background light))
			                       (:bold t :foreground "mediumslateblue"))) t) ; "red4" by default
                         )
                        (ansi-color-for-comint-mode-on)
                        (turn-on-font-lock)
                        (flycheck-mode 1))))


;:*=======================
;:* smex
(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command)))


;:*=======================
;:* swiper
(use-package swiper
  :after ivy
  :bind (("C-r" . swiper-isearch-backward)
         ("C-s" . swiper))
  :config
  (let ((binding (kbd "C-t")))
    (dolist (km (list isearch-mode-map
                      swiper-map
                      swiper-isearch-map))
      (define-key km binding 'swiper-isearch-toggle))))


;:*=======================
;:* terraform
(use-package terraform-mode
  :if (seh-activation-name-in-effect-p "cloud/terraform")
  :config
  (setq
   terraform-format-on-save t)
  :hook (terraform-mode . outline-minor-mode))


;:*=======================
;:* text
(use-package text-mode
  :ensure nil
  :hook (text-mode . (lambda ()
                        (turn-on-auto-fill))))


;:*=======================
;:* treesit-auto
(use-package treesit-auto
  :hook (after-init . global-treesit-auto-mode)
  :config
  ;; See https://github.com/renzmann/treesit-auto/pull/63/files#diff-ecbc1aa90e9ff97a00b0b2aab1551bceee0c4d21993146bdcb1af4de31c9cac6R144-R151.
  (dolist (m '(yaml))
    (delete m treesit-auto-langs)))


;:*=======================
;:* typst-ts-mode
(use-package typst-ts-mode
  :defer t
  :functions (typst-ts-tmenu)
  :config
  (define-key typst-ts-mode-map (kbd "C-c C-c") #'typst-ts-tmenu))


;:*=======================
;:* yaml
(use-package yaml-mode
  :mode "K\\(?:pt\\|rm\\)file\\'"
  :commands (yaml-indent-line))


;:*=======================
;:* yaml-pro
(use-package yaml-pro
  :hook (yaml-mode . yaml-pro-ts-mode))


;:*=======================
;:* yaml-ts-mode
(use-package yaml-ts-mode
  :hook (yaml-ts-mode . (lambda ()
                          (setq-local indent-line-function #'yaml-indent-line))))


;:*=======================
;:* yasnippet
(use-package yasnippet
  :hook ((go-mode
          go-ts-mode
          lua-mode
          lua-ts-mode) . yas-minor-mode))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "packages initialized")
