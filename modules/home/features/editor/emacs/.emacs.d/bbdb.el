;;; -*- lexical-binding: t -*-
;:* bbdb-seh.el
;:*=======================
(use-package bbdb
  :defer t
  :functions (bbdb-initialize)
  :init
  ;; Reading or composing mail must find BBDB's hooks already in place.
  (dolist (feature '(gnus message))
    (with-eval-after-load feature (require 'bbdb)))
  :config
  (bbdb-initialize 'gnus 'message)

  (setq
   bbdb-layout 'multi-line
   bbdb-pop-up-layout 'one-line
   bbdb-completion-display-record nil
   bbdb-complete-mail-allow-cycling t
   bbdb-completion-list '(fl-name primary)
   bbdb-mail-avoid-redundancy nil
   bbdb-mail-user-agent 'message-user-agent
   bbdb-default-area-code 412

   bbdb-notice-record-hook '(bbdb-auto-notes)

   bbdb-auto-notes-rules (list '("Subject" (".*" last-subj 0 t))
                               '("Organization" (".*" organization 0))
                               '("Newsgroups" ("[^,]+" newsgroups 0)))))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "bbdb initialized")
