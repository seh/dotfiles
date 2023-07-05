;:* macos.el
;:*=======================

;; I'm not ready to accept the separation of the Emacs kill ring and
;; the system clipboard.
;; `osxkeys' sets this to nil by default. Force its value back.
(setq-default select-enable-clipboard t)
(setq mac-command-modifier 'meta)

;; TODO(seh): Port aquamacs-page-down-extend-region and aquamacs-page-up-extend-region?
;; See: https://github.com/aquamacs-emacs/aquamacs-emacs/blob/7af9e2a42348c8bc685d504834600b1e26e29a1e/aquamacs/src/site-lisp/aquamacs-editing.el#L234-L242.
;(define-key global-map [(control ?v)] #'scroll-down-maintain-mark)
;(define-key global-map [(meta ?v)] #'scroll-up-maintain-mark)
(define-key global-map [(meta ?`)] 'other-frame)

(defun other-frame-backward (&optional n)
  "Select Nth previous frame"
  (interactive "p")
  (other-frame (- n)))

(define-key global-map [(meta ?~)] 'other-frame-backward)

;; TODO(seh): Set default text face to the "DejaVu Sans Mono" family
;; at height 120.
