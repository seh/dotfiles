;;; -*- lexical-binding: t -*-
;* ~/.emacs
;:*=======================


(defconst user-init-dir
  (cond ((and (not (boundp 'aquamacs-version))
	      ;; Aquamacs uses ~/Library/Preference/Aquamacs Emacs/.
	      (boundp 'user-emacs-directory))
	 user-emacs-directory)
	((boundp 'user-init-file)
	 (file-name-directory user-init-file))
	((boundp 'user-init-directory)
	 user-init-directory)
	(t "~/.emacs.d/")))

(defun load-user-file (file)
  "Load FILE, a base name without suffix, from the configuration directory.
The `load' function prefers a compiled file beside the source."
  (interactive
   (list (file-name-sans-extension
          (file-relative-name
           (read-file-name "Load user file: " user-init-dir nil t nil
                           (lambda (name) (string-suffix-p ".el" name)))
           user-init-dir))))
  (load (expand-file-name file user-init-dir)))

;:*=======================
(load-user-file "activation-names")
(load-user-file "activation")
;; The asynchronous native compiler works in a fresh process; give it
;; the same two files, so compile-time checks see what the build saw.
(defvar native-comp-async-env-modifier-form)
(setq native-comp-async-env-modifier-form
      `(progn (load ,(expand-file-name "activation-names" user-init-dir))
              (load ,(expand-file-name "activation" user-init-dir))))

(declare-function seh-activation-name-in-effect-p "activation")

;:*=======================
(load-user-file "packages")

;:*=======================
(load-user-file "personal")

;:*=======================
;(load-user-file "cygwin")

;:*=======================
(load-user-file "variables")

;:*=======================
(load-user-file "paths")

;:*=======================
(load-user-file "mail-news")

;:*=======================
(load-user-file "misc-funcs")

;:*=======================
(load-user-file "bbdb")

;:*=======================
(load-user-file "calendar")

;:*=======================
(load-user-file "ediff")

;; TODO: gnus-funcs

;:*=======================
(load-user-file "lsp")

;:*=======================
(load-user-file "c-and-java")

;:*=======================
(when (seh-activation-name-in-effect-p "lang/go")
  (load-user-file "go"))

;:*=======================
(when (seh-activation-name-in-effect-p "lang/javascript")
  (load-user-file "javascript"))

;:*=======================
(load-user-file "lisp")

;:*=======================
(when (seh-activation-name-in-effect-p "lang/lua")
  (load-user-file "lua"))

;:*=======================
;(load-user-file "clojure")

;:*=======================
(when (seh-activation-name-in-effect-p "lang/markdown")
  (load-user-file "markdown"))

;:*=======================
(when (seh-activation-name-in-effect-p "lang/rust")
  (load-user-file "rust"))

;:*=======================
(when (seh-activation-name-in-effect-p "lang/zig")
  (load-user-file "zig"))

;:*=======================
(load-user-file "tex")

;:*=======================
(load-user-file "org")

;:*=======================
(load-user-file "fonts-basic")

;:*=======================
;(load-user-file "fonts")

;:*=======================
(load-user-file "frame")

;:*=======================
(load-user-file "server")

;:*=======================
(load-user-file "keys")

;:*=======================
(load-user-file "macos")

;; TODO: Move this out to packaegs.el or something.
(require 'ehelp)
(define-key global-map "\C-h" 'ehelp-command)

;; Preclude Emacs from writing into this file when saving
;; customizations.
(setq custom-file (expand-file-name "custom.el" (file-name-directory user-init-file)))
(load custom-file t) ; Tolerate the file not existing.
