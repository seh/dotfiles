;;; -*- lexical-binding: t -*-
;:* calendar.el
;:*=======================
(eval-after-load "calendar"
  '(progn
     ;; for some reason, these don't work in calendar-load-hook.
     (setq calendar-mark-diary-entries-flag t
           calendar-mark-holidays-flag t)
     (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
     (setq diary-display-function 'diary-fancy-display)
     (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
     (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

     (setq calendar-latitude [40 28 north]
	   calendar-longitude [79 57 west]
	   calendar-location-name "Pittsburgh, PA")))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "calendar initialized")
