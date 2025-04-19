;:* lsp.el
;:*=======================
;;;** Language Server Protocol-related packages

;; LSP configuration for use with Go and other languages
;;
;; See a much more complete example here:
;; https://gist.github.com/psanford/b5d2689ff1565ec7e46867245e3d2c76
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config (progn
            (setq lsp-prefer-flymake nil ;; Use flycheck instead of flymake.
                  lsp-go-use-gofumpt t
                  ;; NB: This only works well with Bazel-based workspaces.
                  ;lsp-go-env `((GOPACKAGESDRIVER . ,(expand-file-name "gopackagesdriver")))
                  )
            (lsp-register-custom-settings
             '(("gopls.staticcheck" t t)))
            ;; NB: I'd prefer to catch directory names more like
            ;; \"/var/folders/mn/cj3m9hf50sd994zln_s_fym40000gn/T/jj-resolve-q0MG7E\",
            ;; but lsp-mode doesn't match these entries as regular expressions.
            ;;
            ;; Basis of inspiration: https://github.com/emacs-lsp/lsp-mode/discussions/4387#discussioncomment-8878188
            ;; TODO(seh): Figure out how to block use of these directories again.
            ;(cl-pushnew (getenv "TMPDIR") (lsp-session-folders-blocklist (lsp-session))
            ;            :test 'string=)
            (dolist (p (list "[/\\\\]bazel-\\w+$"
                             (format "/private/var/tmp/_bazel_%s/" (user-login-name))
                             ;; This one didn't seem to work, even
                             ;; though it matches the paths we'd like
                             ;; to exclude.
                             ;;(format "/private/var/tmp/_bazel_%s/[[:xdigit:]]+/external/" (user-login-name))
                             ))
              (add-to-list 'lsp-file-watch-ignored-directories p t))))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config (setq lsp-ui-doc-header t
                lsp-ui-doc-include-signature t
                lsp-ui-doc-show-with-cursor t
                lsp-ui-doc-show-with-mouse t
                lsp-ui-sideline-show-code-actions t))

(use-package company
  :config (setq company-idle-delay 0
                company-minimum-prefix-length 1
                company-tooltip-align-annotations t))
