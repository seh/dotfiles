;;; -*- lexical-binding: t -*-
;:* calendar.el
;:*=======================
(use-package calendar
  :defer t
  :defines (calendar-latitude
            calendar-location-name
            calendar-longitude
            diary-display-function)
  :config
  (setq calendar-mark-diary-entries-flag t
        calendar-mark-holidays-flag t)
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
  (setq diary-display-function 'diary-fancy-display)
  (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
  (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

  (setq calendar-latitude [40 28 north]
        calendar-longitude [79 57 west]
        calendar-location-name "Pittsburgh, PA"))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "calendar initialized")
