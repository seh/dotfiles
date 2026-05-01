;:* org.el
;:*=======================
;;;** 'org-mode' package

(use-package org
  :mode (("\\.org\\'" . org-mode))
  :config
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
        ;; TODO(seh): Customize org-todo-keyword-faces.
        org-log-done '(done)
        org-hide-leading-stars t
        org-startup-indented t
        org-agenda-include-diary t

        org-default-notes-file (concat org-directory "/notes.org")
        org-capture-templates
        '(("t" "Todo" entry (file+headline nil "Tasks")
           "* TODO %?\n  %u\n  %a\n  %i\n")
          ("j" "Journal" entry (file+olp+datetree "journal.org")
           "* %?\nEntered on %U\n%a\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n"))))

;; TODO(seh): Can we integrate these into the "use-package" form
;; above?
(define-key mode-specific-map [?a] 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(use-package org-edit-indirect
  :hook (org-mode . org-edit-indirect-mode))

(use-package org-modern
  :config
  (global-org-modern-mode))

(use-package org-roam
  :config
  (setq org-roam-directory (expand-file-name "org-roam" org-directory))
  (org-roam-setup)
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n l" . org-roam-buffer-toggle)))

(use-package org-roam-timestamps
  :after org-roam
  :config
  (org-roam-timestamps-mode))


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
