;;; -*- lexical-binding: t -*-
;:* rust.el
;:*=======================
(use-package rust-mode
  :defines (rust-mode-map)
  :functions (flycheck-tip-cycle)
  :preface
  (defun seh-rust-mode-hook ()
    ;; TODO: Sniff out a likely package ID.
    (when (and (locate-dominating-file default-directory "Cargo.toml")
               (executable-find "cargo"))
      (setq-local compile-command "cargo build")))
  :hook (rust-mode . seh-rust-mode-hook)
  :bind (:map rust-mode-map
              ("C-c C-n" . flycheck-tip-cycle))
  :config
  (require 'flycheck-tip))

(use-package rustic
  :defer t
  :defines (rustic-format-trigger
            rustic-lsp-client)
  :config
  (setq rustic-format-trigger 'on-save)
  ;; Without a language-server client installed, rustic offers to
  ;; install one from a package archive in every Rust buffer.
  (unless (seh-activation-name-in-effect-p "dev/language-servers")
    (setq rustic-lsp-client nil)))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "Rust settings initialized")
