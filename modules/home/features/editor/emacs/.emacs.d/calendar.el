;:* calendar.el
;:*=======================
(eval-after-load "calendar"
  '(progn
     ;; for some reason, these don't work in calendar-load-hook.
     (setq diary-file "~/.diary"
           mark-diary-entries-in-calendar t
           mark-holidays-in-calendar t)
     (add-hook 'today-visible-calendar-hook 'calendar-mark-today)
     (add-hook 'diary-display-hook 'fancy-diary-display)
     (add-hook 'list-diary-entries-hook 'include-other-diary-files)
     (add-hook 'mark-diary-entries-hook 'mark-included-diary-files)

     (setq calendar-latitude [40 28 north]
	   calendar-longitude [79 57 west]
	   calendar-loaction-name "Pittsburgh, PA")))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "calendar initialized")
