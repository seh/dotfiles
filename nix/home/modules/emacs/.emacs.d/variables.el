;:* variables.el
;:*=======================
(defconst SEH-tab-width 4)

(setq-default
 column-number-mode t
 indent-tabs-mode nil
 make-backup-files nil ; Aquamacs disables this by default.
 truncate-lines t)

(setq show-paren-style 'parenthesis) ; or 'mixed
(show-paren-mode t)

(setq inhibit-startup-screen t)
;(fringe-mode '(0 . nil))
(tab-bar-mode 0)
(tool-bar-mode 0)
(transient-mark-mode 1)
(delete-selection-mode 1)

(setq compilation-ask-about-save nil)
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "variables initialized")
