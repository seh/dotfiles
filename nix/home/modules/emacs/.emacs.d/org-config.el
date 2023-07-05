;:* org.el
;:*=======================
;;;** 'org-mode' package

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(define-key mode-specific-map [?a] 'org-agenda)

(setq org-directory "~/Documents"
      org-catch-invisible-edits 'smart
      org-special-ctrl-a/e t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-start-on-weekday nil
      org-todo-keywords '((sequence "TODO(t)"
                                    "STARTED(s!)"
                                    "WAITING(w@/!)"
                                    "DELEGATED(g@/!)"
                                    "APPT(a@)"
                                    "|"
                                    "DONE(d!)"
                                    "DEFERRED(f@)"
                                    "CANCELLED(c@)"))
      org-use-fast-todo-selection t
      org-use-speed-commands t
      ;; TODO: Customize org-todo-keyword-faces.
      org-log-done '(done)
      org-hide-leading-stars t
      org-startup-indented t
      org-agenda-include-diary t

      org-default-notes-file (concat org-directory "/notes.org")
      org-capture-templates
      '(("t" "Todo" entry (file+headline nil "Tasks")
         "* TODO %?\n  %u\n  %a\n  %i\n")
        ("j" "Journal" entry (file+datetree "journal.org")
         "* %?\nEntered on %U\n%a\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n")))

(define-key global-map "\C-cc" 'org-capture)


;; (require 'remember)
;; (setq remember-annotation-functions '(org-remember-annotation)
;;       remember-handler-functions '(org-remember-handler)
;;       ;; org-remember-store-without-prompt t
;;       )
;; (add-hook 'remember-mode-hook 'org-remember-apply-template)

;; (setq org-remember-templates
;;       '((?t "** TODO %?\n  %i\n  %a" "~/doc/org/personal.org" "Tasks")))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "org-mode settings initialized")
