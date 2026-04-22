;:* markdown.el
;:*=======================
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(setq markdown-italic-underscore t)
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "Markdown settings initialized")
