;:* packages.el
;:*=======================
;; * The `package' system itself
(require 'package)

;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)))
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; Basis of inspiration:
;;   https://github.com/bdd/.emacs.d/blob/master/packages.el

;; If never connected to repositories before, download package
;; descriptions so `use-package' can trigger installation of missing
;; packages.
(unless package-archive-contents
  (message "Refreshing ELPA package archives...")
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (message "`use-package' not found. Installing...")
  (package-install 'use-package))

(require 'use-package)


;:*=======================
;:* beacon
(use-package beacon
  :config
  (setq
   ;; This is 0.3 by default. Values lower than 0.2 don't appear to
   ;; make a difference.
   beacon-blink-duration 0.2)
  (beacon-mode 1))


;:*=======================
;:* boxquote
(use-package boxquote)


;:*=======================
;:* counsel
(use-package counsel
  :after ivy
  :config
  (counsel-mode))


;:*=======================
;:* cue
(use-package cue-mode)


;:*=======================
;:* dired
;; TODO: Bind 'K' to dired-kill-subdir

;; This per http://pragmaticemacs.com/emacs/tree-style-directory-views-in-dired-with-dired-subtree/:
(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))


;:*=======================
;:* diary
(setq diary-file "~/.diary")


;:*=======================
;:* doom-modeline
(use-package nerd-icons)

;; Basis of inspiration: https://config.daviwil.com/emacs#doom-modeline
(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-minor-modes t)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-lsp t))


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
;:* gnus
;; Unless we use `custom-set-variables' here, we can't wait until
;; the package is loaded to set these, as other custom-based variables
;; depend upon them.
(setq gnus-directory "~/doc/news"
      message-directory gnus-directory)
(eval-after-load "gnus"
  '(progn
     (setq gnus-kill-files-directory gnus-directory)
    (require 'gnus-dired)
    (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)))
;; the rest of the configuration is in ~/.gnus


;:*=======================
;:* ido
;; TODO: Port more of this:
;;       https://github.com/magit/magit/issues/2017#issuecomment-120406236
(use-package ido
  :config
  (ido-mode)
  (ido-everywhere)
  (setq ido-use-virtual-buffers t)
  (let ((alternate-method 'maybe-frame))
    (dolist (v '(ido-default-buffer-method
                 ido-default-file-method))
      (set-variable v alternate-method)))
  :hook (ido-setup . (lambda ()
                       (let ((kmap ido-file-dir-completion-map))
                         (let ((key '(meta ?n)))
                           (define-key kmap (vector (cons 'control key))
                                       (lookup-key kmap (vector key)))
                           (define-key kmap (vector key) 'ido-next-work-file))
                         (let ((key '(meta ?p)))
                           (define-key kmap (vector (cons 'control key))
                                       (lookup-key kmap (vector key)))
                           (define-key kmap (vector key) 'ido-prev-work-file))))))
(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode))


;:*=======================
;:* iedit
(use-package iedit)


;:*=======================
;:* ispell
(use-package ispell)


;:*=======================
;:* ivy
(use-package ivy
  :config
  ;; This same function is available via "C-M-j" as well, but that's
  ;; harder to type. By default, "M-RET" is bound to "ivy-call", which
  ;; isn't as useful.
  (define-key ivy-minibuffer-map (kbd "M-RET") #'ivy-immediate-done)
  (ivy-mode))


;:*=======================
;:* magit
(use-package magit
  :config
  (setq magit-popup-use-prefix-argument 'default
        ;; Edit jj commit messages using "git-commit-mode":
        ;; Default value: "/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'"
        git-commit-filename-regexp "/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\|\\.jjdescription\\'"
        git-commit-summary-max-length 50))


;:*=======================
;:* nix
(use-package nix
  :hook
  (nix-mode . electric-pair-mode)
  (nix-mode . (lambda ()
                (add-hook 'before-save-hook #'nix-format-before-save 0 t))))


;:*=======================
;:* persistent-scratch
(use-package persistent-scratch
  :demand t
  :hook (after-init . (lambda ()
                        (when (file-exists-p persistent-scratch-save-file)
                          (persistent-scratch-restore))
                        (with-current-buffer "*scratch*"
                          (persistent-scratch-mode)))))

;:*=======================
;:* prettier
(use-package prettier
  :hook ((js-base-mode . prettier-mode)
         (typescript-ts-base-mode . prettier-mode)))


;:*=======================
;:* project
(use-package project
  :config
  (add-to-list 'project-vc-extra-root-markers ".jj"))


;:*=======================
;:* recentf
(use-package recentf
  :hook (buffer-list-update . recentf-track-opened-file))


;:*=======================
;:* rg
(use-package rg
  :config
  (rg-enable-menu))

;:*=======================
;:* rustic
(use-package rustic
  :config
  (setq rustic-format-trigger 'on-save))


;:*=======================
;:* sh-mode
(use-package sh-script
  :config
  (setq sh-basic-offset 2
        sh-indentation 2))


;:*=======================
;:* shell
(use-package shell
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
  :config
  ;; See https://github.com/renzmann/treesit-auto/pull/63/files#diff-ecbc1aa90e9ff97a00b0b2aab1551bceee0c4d21993146bdcb1af4de31c9cac6R144-R151.
  (dolist (m '(yaml))
    (delete m treesit-auto-langs))
  (global-treesit-auto-mode))


;:*=======================
;:* typescript-ts-mode
(use-package typescript-ts-mode
  :hook (typescript-ts-base-mode . (lambda ()
                                     (setq js-indent-level 2)
                                     (electric-pair-local-mode)
                                     (lsp-deferred)
                                     (lsp-lens-mode)
                                     (dolist (h '(lsp-format-buffer
                                                  lsp-organize-imports))
                                       (add-hook 'before-save-hook h nil t)))))


;:*=======================
;:* yaml
(use-package yaml-mode
  :mode "K\\(?:pt\\|rm\\)file\\'")

;:*=======================
;:* yaml-pro
(use-package yaml-pro
  :hook (yaml-mode . yaml-pro-ts-mode))


;; TODO: Migrate more of these in.

;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "packages initialized")
