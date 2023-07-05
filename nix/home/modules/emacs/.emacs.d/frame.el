;:* frame.el
;:*=======================
(defun frame-x-offset (&optional frame)
  (let ((pos (frame-parameter (or frame (selected-frame)) 'left)))
    (if (consp pos)
        ;; Assume the car is '+.
        (- (second pos))
      pos)))


(defconst +taskbar-pixel-height+ 57)
(defconst +desired-frame-y-offset+ 0)
(defconst +device-pixel-height+ 1050) ; No analog for device-pixel-height.
(defconst +desired-frame-height+ (- +device-pixel-height+
                                    +taskbar-pixel-height+
                                    +desired-frame-y-offset+))

(defun maximize-frame-height (&optional frame)
  (interactive)
  (let ((frame (or frame (selected-frame))))
    (set-frame-height frame (ceiling +desired-frame-height+
                                     (frame-char-height frame)))
    (set-frame-position frame
                        (frame-x-offset frame)
                        +desired-frame-y-offset+)))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "frame settings initialized")
