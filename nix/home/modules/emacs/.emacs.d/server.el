;:* server.el
;:*=======================
;; See http://www.emacswiki.org/cgi-bin/wiki/EmacsClient#toc24.
(setq server-window 'switch-to-buffer-other-frame)
(add-hook 'server-done-hook 'delete-frame)
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "server settings initialized")
