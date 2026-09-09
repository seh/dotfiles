;;; -*- lexical-binding: t -*-
;:* fonts-basic.el
;:*=======================

(setq-default cursor-type '(bar . 2))
(blink-cursor-mode 0)


;:* describe-face-at-point, a function to find out which face is which
(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (describe-face (get-char-property (point) 'face)))


(eval-after-load "font-lock"
  '(progn
     (setq-default
      font-lock-auto-fontify t
      font-lock-maximum-decoration t)))

;:* Various faces follow that we wish to tailor even using color themes.
(custom-set-faces
 ;;:* normal text
 '(default
   ((((class color))
     (:family "Monaspace Neon"
      ;; For the "default" face, we must use a fixed value. We cannot
      ;; compute it like this:
      ;;  (floor (face-attribute 'default :height)
      ;;    text-scale-mode-step)
      ;:height 114
              ))) t)
 )

;; Themes reset these faces, so apply the adjustments now and again
;; whenever a theme is enabled.
(defun seh-tailor-faces (&rest _)
  "Apply my adjustments to the faces a theme resets."
  (dolist (face '(font-lock-function-name-face
                  font-lock-string-face))
    (make-face-bold face))
  (when (featurep 'message)
    (dolist (face '(message-header-to
                    message-header-cc))
      (make-face-bold face))))

(seh-tailor-faces)
(add-hook 'enable-theme-functions #'seh-tailor-faces)
(with-eval-after-load 'message (seh-tailor-faces))

;:* eldoc-mode
(eval-after-load "eldoc"
  '(progn
     (custom-set-faces
      '(eldoc-highlight-function-argument
        ((((class color))
          (:foreground "tomato" :bold t))) t)
      )))
