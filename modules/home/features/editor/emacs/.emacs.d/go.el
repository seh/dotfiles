;;; -*- lexical-binding: t -*-
;:* go.el
;:*=======================
;;;** 'go-mode' package

(declare-function seh-activation-name-in-effect-p "activation")

(defvar SEH-tab-width)

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
                                  (when (seh-activation-name-in-effect-p "lang/go/ls")
                                    (dolist (h '(lsp-format-buffer
                                                 lsp-organize-imports))
                                      (add-hook 'before-save-hook h nil t))))))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "Go settings initialized")
