;:* server.el
;:*=======================
;; See http://www.emacswiki.org/cgi-bin/wiki/EmacsClient#toc24.
(custom-set-variables
 '(server-switch-hook
   (lambda ()
     (let ((server-buf (current-buffer)))
       (bury-buffer)
       (switch-to-buffer-other-frame server-buf))))
 '(server-done-hook
   (lambda ()
     (delete-frame))))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "server settings initialized")
