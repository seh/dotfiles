;:* go.el
;:*=======================
;;;** 'go-mode' package

;; LSP configuration for use with Go
;;
;; See a much more complete example here:
;; https://gist.github.com/psanford/b5d2689ff1565ec7e46867245e3d2c76
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :config (progn
            (setq lsp-prefer-flymake nil ;; Use flycheck instead of flymake.
                  lsp-go-use-gofumpt t
                  ;; NB: This only works well with Bazel-based workspaces.
                  ;lsp-go-env `((GOPACKAGESDRIVER . ,(expand-file-name "gopackagesdriver")))
                  )
            (lsp-register-custom-settings
             '(("gopls.completeUnimported" t t)
               ;("gopls.usePlaceholders" t t)
               ("gopls.staticcheck" t t)))
            ;; NB: This is called "lsp-file-watch-ignored-directories"
            ;; in newer versions of lsp-mode.
            (dolist (p (list "[/\\\\]bazel-\\w+$"
                             (format "/private/var/tmp/_bazel_%s/" (user-login-name))
                             ;; This one didn't seem to work, even
                             ;; though it matches the paths we'd like
                             ;; to exclude.
                             ;;(format "/private/var/tmp/_bazel_%s/[[:xdigit:]]+/external/" (user-login-name))
                             ))
              (add-to-list 'lsp-file-watch-ignored p t))))

;; optional - provides fancier overlays
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config (progn
            (setq lsp-ui-doc-header t
                  lsp-ui-doc-include-signature t
                  lsp-ui-doc-show-with-cursor t
                  lsp-ui-doc-show-with-mouse t)
                  lsp-ui-sideline-show-code-actions t))

(use-package company
  :ensure t
  :config (progn
            (setq company-idle-delay 0)
            (setq company-minimum-prefix-length 1)
            (setq company-tooltip-align-annotations t)))

(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(use-package go-mode
  :ensure t
  :bind (
         ;; If you want to switch existing go-mode bindings to use lsp-mode/gopls instead
         ;; uncomment the following lines
         ;; ("C-c C-j" . lsp-find-definition)
         ;; ("C-c C-d" . lsp-describe-thing-at-point)
         )
  :hook ((go-mode . (lambda ()
                      (setq tab-width SEH-tab-width)
                      (setq fill-column 100)
                      (electric-pair-mode 1)
                      (lsp-deferred)
                      (lsp-lens-mode)
                      (dolist (h '(lsp-format-buffer
                                   lsp-organize-imports))
                        (add-hook 'before-save-hook h nil t))))))

;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "Go settings initialized")
