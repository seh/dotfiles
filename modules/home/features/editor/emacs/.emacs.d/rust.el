;;; -*- lexical-binding: t -*-
;:* rust.el
;:*=======================
(eval-after-load "rust-mode"
  '(progn
     (require 'flycheck-tip)
     (define-key rust-mode-map (kbd "C-c C-n") 'flycheck-tip-cycle)

     (add-hook 'rust-mode-hook
               ;; TODO: Sniff out a likely package ID.
               (lambda ()
                 (when (and (locate-dominating-file default-directory "Cargo.toml")
                            (executable-find "cargo"))
                   (set (make-local-variable 'compile-command)
                        "cargo build"))))))

(use-package rustic
  :config
  (setq rustic-format-trigger 'on-save)
  ;; Without a language-server client installed, rustic offers to
  ;; install one from a package archive in every Rust buffer.
  (unless (seh-activation-name-in-effect-p "dev/language-servers")
    (setq rustic-lsp-client nil)))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "Rust settings initialized")
