;;; -*- lexical-binding: t -*-
;:* mail-news.el
;:*=======================
;; Use local MTA configuration to handle external identity.
;(setq user-mail-address "seh@panix.com")
;(setq sendmail-program "postfix")
(setq send-mail-function 'sendmail-send-it
      message-send-mail-function 'sendmail-send-it)


(eval-after-load "message"
  '(progn
     (setq
      message-directory "~/doc/mail"
      mail-source-directory message-directory
      mail-user-agent 'gnus-user-agent
      message-cite-function 'message-cite-original-without-signature
      ;message-kill-buffer-on-exit t
      ;mail-host-address "Spindle.sehlabs.com"
      )

     (let ((map message-mode-map))
       ;; F8 is bound to the Spaces activation operation.
       (define-key map [f6] (lambda ()
                             (interactive)
                             (save-excursion
                               (message-remove-header "Gcc"))))
       (define-key map [f7] 'ispell-message)
       (define-key map [(control c) (control a)] 'mail-interactive-insert-alias))))


(defun SEH-message-mode-hook ()
  (turn-on-auto-fill))

(add-hook 'message-mode-hook 'SEH-message-mode-hook t)
(add-hook 'message-setup-hook 'bbdb-mail-aliases)
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "mail and news settings initialized")
