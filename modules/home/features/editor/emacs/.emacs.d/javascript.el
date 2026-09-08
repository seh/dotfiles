;;; -*- lexical-binding: t -*-
;:* javascript.el
;:*=======================
(use-package prettier
  :if (seh-activation-name-in-effect-p "lang/javascript/tools")
  :init
  ;; Without this, prettier-mode has trouble finding the "prettier"
  ;; program, even though it's already available on the path.
  (setenv "NODE_PATH" (expand-file-name "~/.nix-profile/lib/node_modules"))
  :hook ((js-base-mode . prettier-mode)
         (typescript-ts-base-mode . prettier-mode)))

(use-package typescript-ts-mode
  :hook (typescript-ts-base-mode . (lambda ()
                                     (setq js-indent-level 2)
                                     (electric-pair-local-mode)
                                     (when (seh-activation-name-in-effect-p "lang/javascript/ls")
                                       (dolist (h '(lsp-format-buffer
                                                    lsp-organize-imports))
                                         (add-hook 'before-save-hook h nil t))))))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "JavaScript settings initialized")
