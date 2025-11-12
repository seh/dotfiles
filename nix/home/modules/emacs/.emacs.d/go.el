;:* go.el
;:*=======================
;;;** 'go-mode' package

(use-package go-mode
  :bind (
         ;; If you want to switch existing go-mode bindings to use lsp-mode/gopls instead
         ;; uncomment the following lines
         ;; ("C-c C-j" . lsp-find-definition)
         ;; ("C-c C-d" . lsp-describe-thing-at-point)
         )
  :hook ((go-mode go-ts-mode) . (lambda ()
                                  (setq
                                   tab-width SEH-tab-width
                                   fill-column 100)
                                  (electric-pair-local-mode)
                                  (dolist (h '(lsp-format-buffer
                                               lsp-organize-imports))
                                    (add-hook 'before-save-hook h nil t)))))

(use-package yasnippet
  :hook ((go-mode go-ts-mode) . yas-minor-mode))

;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "Go settings initialized")
