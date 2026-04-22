;:* color-theme.el
;:*=======================

;; NB: Most of this file was dedicated to the package mentioned below,
;; that's neither readily available nor functioning properly now
;; (early 2020). Here are a few concessions for using the "sanityinc"
;; variant.

;; Adapted from https://github.com/pkkm/.emacs.d/blob/94b49aae2e48d1a54bac05f2285629a49e1a4e7d/conf/view/color-theme.el#L3-L9:
(defvar after-enable-theme-hook nil
  "Hook run after a color theme is enabled using `enable-theme'.")
(defadvice enable-theme (after run-after-enable-theme-hook activate)
  "Run `after-enable-theme-hook'."
  (run-hooks 'after-enable-theme-hook))

(eval-after-load "font-lock"
  '(progn
     (defun fix-up-basic-fonts ()
       (dolist (face '(font-lock-function-name-face
                       font-lock-string-face))
         (make-face-bold face)))
     (add-hook 'after-enable-theme-hook #'fix-up-basic-fonts)))

(eval-after-load "message"
  '(progn
     (defun fix-up-message-fonts ()
       (dolist (face '(message-header-to
                       message-header-cc))
         (make-face-bold face)))
     (add-hook 'after-enable-theme-hook #'fix-up-message-fonts)))


;; NB: This package seems to have disappeared from MELPA.
;(load-theme 'solarized t)
(add-to-list 'load-path "~/src/emacs-color-theme-solarized")
(require 'color-theme-solarized)
(require 'solarized-theme)
;; TODO(seh): Remove this.
;(add-to-list 'default-frame-alist '(background-mode . light))
;(set-terminal-parameter nil 'background-mode 'light)
;(terminal-parameter nil 'background-mode)
;(frame-parameter nil 'background-mode)
;(setq frame-background-mode 'light)
;(setq frame-background-mode 'dark)
;(color-theme-solarized)
;(mapc 'frame-set-background-mode (frame-list))
                                        ;(enable-theme 'solarized)

(defun solarized-color-by-name (name)
  ;; This is not as sophisticated as `find-color' in
  ;; solarized-definitions.el, which takes into account how to
  ;; degrade appropriately. Here we just assume that an sRGB is
  ;; acceptable.
  (nth 1 (assoc name solarized-colors)))

(defvar *cursor-color*
  (solarized-color-by-name 'yellow))  ; or maybe orange, or base2

;; Per https://github.com/sellout/emacs-color-theme-solarized#all-versions:
(add-hook 'after-make-frame-functions
          (lambda (frame)
            ;; (let ((mode (if (display-graphic-p frame) 'light 'dark)))
            ;;   (set-frame-parameter frame 'background-mode mode)
            ;;   (set-terminal-parameter frame 'background-mode mode))
            (enable-theme 'solarized)
            ;; NB: `set-cursor-color' normally works for the current
            ;; frame, but it doesn't seem to have any effect when run
            ;; in this hook.
            (set-face-attribute 'cursor nil :background *cursor-color*)))

(defun toggle-night-color-theme ()
    "Switch to/from night color scheme."
    (interactive)
    (setq frame-background-mode
          (if (eq frame-background-mode 'dark) 'light 'dark))
    ;;(enable-theme 'solarized)
    (load-theme 'solarized t)
    ;; TODO(seh): These lines were transposed.
    (mapc 'frame-set-background-mode (frame-list))
    (set-cursor-color *cursor-color*))

;; Now layer back in a few customizations that I've been using for years:
(set-cursor-color *cursor-color*)

(eval-after-load "font-lock"
  '(dolist (face '(font-lock-function-name-face
                   font-lock-string-face))
     (make-face-bold face)))

(eval-after-load "message"
  '(dolist (face '(message-header-to
                   message-header-cc))
     (make-face-bold face)))

(eval-after-load "gnus"
  '(progn
     (let ((color (solarized-color-by-name 'base0)))
       (dolist (face '(gnus-summary-low-unread
                       gnus-summary-normal-unread
                       gnus-summary-high-unread))
         (set-face-foreground face color)))
     (let ((color (solarized-color-by-name 'base01)))
       (dolist (face '(gnus-summary-low-read
                       gnus-summary-normal-read
                       gnus-summary-high-read))
         (set-face-foreground face color)))
     (let ((color (solarized-color-by-name 'blue)))
       (dolist (face '(gnus-summary-low-undownloaded
                       gnus-summary-normal-undownloaded
                       gnus-summary-high-undownloaded))
         (set-face-foreground face color)))
     (let ((color (solarized-color-by-name 'violet)))
       (dolist (face '(gnus-summary-low-ticked
                       gnus-summary-normal-ticked
                       gnus-summary-high-ticked))
         (set-face-foreground face color)))
     (let ((color (solarized-color-by-name 'cyan))) ; was magenta
       (dolist (face '(gnus-group-mail-3
                       gnus-group-mail-3-empty))
         (set-face-foreground face color)))))

(eval-after-load "gnus-cite"
  '(progn
     (let ((color (solarized-color-by-name 'violet))) ; or cyan
       (dolist (face '(gnus-cite-1))
         (set-face-foreground face color)))))

;; NB: There are others that I set to bold elsewhere, but that the
;; Solarized color theme doesn't happen to override yet.
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "Color theme-related settings initialized")
