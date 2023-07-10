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
;:* ispell
(use-package ispell)


;:*=======================
;:* magit
(use-package magit
  :config
  (setq magit-popup-use-prefix-argument 'default
        git-commit-summary-max-length 50))


;:*=======================
;:* nix
(use-package nix
  :hook (nix-mode . electric-pair-mode))


;:*=======================
;:* nixpkgs-fmt
(use-package nixpkgs-fmt
  :hook (nix-mode . nixpkgs-fmt-on-save-mode))


;:*=======================
;:* persistent-scratch
(use-package persistent-scratch
  :demand t
  :config
  (when (file-exists-p persistent-scratch-save-file)
    (persistent-scratch-restore))
  (with-current-buffer "*scratch*"
    (persistent-scratch-mode)))


;:*=======================
;:* recentf
(use-package recentf
  :hook (buffer-list-update . recentf-track-opened-file))


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
;:* text
(use-package text-mode
  :ensure nil
  :hook (text-mode . (lambda ()
                        (turn-on-auto-fill))))


;:*=======================
;:* yaml
(use-package yaml
  :mode "K\\(?:pt\\|rm\\)file\\'")


;; TODO: Migrate more of these in.

;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "packages initialized")
