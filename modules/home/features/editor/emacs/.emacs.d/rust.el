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
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "Rust settings initialized")
