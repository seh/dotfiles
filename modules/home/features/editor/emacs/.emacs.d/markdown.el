;;; -*- lexical-binding: t -*-
;:* markdown.el
;:*=======================
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(defvar markdown-italic-underscore)
(setq markdown-italic-underscore t)
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "Markdown settings initialized")
