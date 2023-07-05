;:* bbdb-seh.el
;:*=======================
(require 'bbdb)
(bbdb-initialize 'gnus 'message)

(setq
 bbdb-offer-save 'savenoprompt
 bbdb-display-layout 'multi-line
 bbdb-pop-up-display-layout 'one-line
 bbdb-completion-display-record nil
 bbdb-complete-name-allow-cycling t
 bbdb-completion-type 'primary-or-name
 bbdb-dwim-net-address-allow-redundancy t
 bbdb-send-mail-style 'message
 bbdb-default-area-code 412

 bbdb-notice-hook 'bbdb-auto-notes-hook

 bbdb-auto-notes-alist (list '("Subject" (".*" last-subj 0 t))
			     '("Organization" (".*" company 0))
			     '("Newsgroups" ("[^,]+" newsgroups 0))))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "bbdb initialized")