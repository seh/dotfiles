;:* keys.el
;:*=======================
(define-key global-map [(control x) (c)] 'copy-current-line)
(define-key global-map [(meta o)] 'other-window-backward)
;(define-key global-map [(meta insert)] 'vi-open-line)
;(define-key help-map "f" 'hyper-describe-function) ; prefer 'hyper' versions?
;(define-key help-map "v" 'hyper-describe-variable)
(define-key global-map [f2] 'maximize-frame-height)
(define-key global-map [f10] 'font-lock-and-fontify)
(define-key global-map [f11] 'line-to-top-of-window)
(define-key global-map [f12] 'toggle-truncate-lines)

(define-key global-map [(shift down)] (lambda (&optional n)
                                        (interactive "p")
                                        (scroll-up (or n 1))))
(define-key global-map [(shift up)] (lambda (&optional n)
                                      (interactive "p")
                                      (scroll-down (or n 1))))

;(define-key global-map [(control shift button3)] 'imenu)
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "key bindings initialized")
