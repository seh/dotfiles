;:* macos.el
;:*=======================

(setq ns-command-modifier 'meta  ;; Aliased by `mac-command-modifier'
      ns-option-modifier 'super) ;; Aliased by `mac-option-modifier', for `ns-alternate-modifier'

(defun toggle-macbook-keyboard-bindings (&optional force)
  "Toggle the bindings of ancillary keys for use with macOS, accommodating
preferences for MacBook Air- and MacBook Pro-style keyboards.

With a prefix argument, bind the keys even if they're not currently
bound as expected."
  (interactive "p")
  (let ((next-binding (cl-case ns-option-modifier
                        (super 'meta)
                        (meta 'super)
                        (t (when force
                             'super)))))
    (if next-binding
        (progn
          (setq ns-option-modifier next-binding)
          (message "Option (⌥) key is now bound to %s." next-binding))
      (message "Option (⌥) key is currently %s; not toggling."
               (or ns-option-modifier "unbound")))))

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
