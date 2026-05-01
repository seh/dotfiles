;:* aquamacs.el
;:*=======================
(cua-mode 0)
(osx-key-mode 0)
;; I'm not ready to accept the separation of the Emacs kill ring and
;; the system clipboard.
;; `osxkeys' sets this to nil by default. Force its value back.
(setq-default select-enable-clipboard t)
(setq mac-command-modifier 'meta)

;; Revert the binding for `recentf-open-files' defined in
;; /Applications/Aquamacs.app/Contents/Resources/lisp/aquamacs/macosx/aquamacs-menu.el:
(eval-after-load "aquamacs-menu"
  '(progn
     (global-set-key [(control ?x) (control ?r)] 'find-file-read-only)))

;; Open *Help* frame in the current frame.
(setq special-display-regexps
      (remove "[ ]?\\*[hH]elp.*" special-display-regexps))

;; Enable scrolling to maintain mark if set
(defun scroll-down-maintain-mark ()
 (interactive)
 (if mark-active
     (aquamacs-page-down-extend-region)
   (aquamacs-page-down)))

(defun scroll-up-maintain-mark ()
 (interactive)
 (if mark-active
     (aquamacs-page-up-extend-region)
   (aquamacs-page-up)))

(define-key global-map [(control ?v)] #'scroll-down-maintain-mark)
(define-key global-map [(meta ?v)] #'scroll-up-maintain-mark)
(define-key global-map [(meta ?`)] 'raise-next-frame)

; In file aquamacs-faces, this face is defined using the "Lucida
;; Grande" family, and then this face gets inherited by several
;; others, eventually impacting message mode's face used during
;; composition (not just reading messages).
(custom-set-faces
 '(text-mode-default
   ((((type ns))
     (:family "DejaVu Sans Mono"
              :height 120))) t))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "Aquamacs-specific settings initialized")
