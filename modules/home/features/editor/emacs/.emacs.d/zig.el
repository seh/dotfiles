;;; -*- lexical-binding: t -*-
;:* zig.el
;:*=======================
(use-package zig-ts-mode
  :defer t)

;; The language server formats on save where it is in effect; without
;; it, run "zig fmt" directly, when the program is available.
(eval-and-compile
  (define-inline seh-zig-formats-without-server-p ()
    "Return non-nil when no Zig language server is in effect.
In that case \"zig fmt\" formats Zig buffers."
    (inline-quote (not (seh-activation-name-in-effect-p "lang/zig/ls"))))
  (when (seh-zig-formats-without-server-p)
    (require 'reformatter)
    (reformatter-define seh-zig-format
      :program "zig"
      :args (append '("fmt" "--stdin")
                    (when (and buffer-file-name
                               (string-suffix-p ".zon" buffer-file-name))
                      '("--zon")))
      :mode nil)))

(when (seh-zig-formats-without-server-p)
  (with-eval-after-load 'zig-ts-mode
    (if (executable-find "zig")
        (add-hook 'zig-ts-mode-hook
                  (lambda ()
                    (add-hook 'before-save-hook #'seh-zig-format-buffer nil t)))
      (display-warning 'seh-zig
                       "No \"zig\" program on the PATH, so Zig buffers will not be formatted on save"))))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "Zig settings initialized")
