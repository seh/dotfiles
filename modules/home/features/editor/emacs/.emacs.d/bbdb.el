;;; -*- lexical-binding: t -*-
;:* bbdb-seh.el
;:*=======================
(require 'bbdb)
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
                             '("Newsgroups" ("[^,]+" newsgroups 0))))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "bbdb initialized")
