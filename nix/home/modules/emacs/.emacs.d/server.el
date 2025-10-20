;:* server.el
;:*=======================
;; See http://www.emacswiki.org/cgi-bin/wiki/EmacsClient#toc24.

(use-package server
  ;; NB: While setting this variable appears to be simpler than
  ;; binding a hook function, it doesn't work correctly with magit's
  ;; management and presentation of buffers.
  ;;(setq server-window 'switch-to-buffer-other-frame)
  :hook ((server-switch . (lambda ()
                            (let ((server-buf (current-buffer)))
                              (bury-buffer)
                              (switch-to-buffer-other-frame server-buf))))
         (server-done . delete-frame)
         ;; NB: We're relying on the Nix Home Manager module to set an
         ;; Emacs daemon running for us.
         ;;(after-init . server-start)
         ))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "server settings initialized")
