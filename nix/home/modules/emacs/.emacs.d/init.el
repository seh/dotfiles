;* ~/.emacs
;:*=======================


(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

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
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

;:*=======================
(load-user-file "packages.el")

;:*=======================
(load-user-file "personal.el")

;:*=======================
(load-user-file "platform.el")

;:*=======================
;(load-user-file "cygwin.el")

;:*=======================
(load-user-file "variables.el")

;:*=======================
(load-user-file "paths.el")

;:*=======================
(load-user-file "mail-news.el")

;:*=======================
(load-user-file "misc-funcs.el")

;:*=======================
(load-user-file "bbdb.el")

;:*=======================
(load-user-file "calendar.el")

;; TODO: gnus-funcs

;:*=======================
(load-user-file "c-and-java.el")

;:*=======================
(load-user-file "go.el")

;:*=======================
(load-user-file "lisp.el")

;:*=======================
;(load-user-file "clojure.el")

;:*=======================
;(load-user-file "sgml-xml.el")

;:*=======================
(load-user-file "markdown.el")

;:*=======================
(load-user-file "rust.el")

;:*=======================
(load-user-file "tex.el")

;:*=======================
(load-user-file "org-config.el")

;:*=======================
(load-user-file "fonts-basic.el")

;:*=======================
;(load-user-file "fonts.el")
 
;:*=======================
;; TODO(seh): Reenable this after we've figured out why the "light" background mode doesn't work.
;(load-user-file "color-theme.el")

;:*=======================
(load-user-file "frame.el")

;:*=======================
(load-user-file "server.el")

;:*=======================
(load-user-file "keys.el")

;:*=======================
(load-user-file "macos.el")

;; TODO: Move this out to packaegs.el or something.
(require 'ehelp)
(define-key global-map "\C-h" 'ehelp-command)

;; Preclude Emacs from writing into this file when saving
;; customizations.
(setq custom-file (expand-file-name "custom.el" (file-name-directory user-init-file)))
(load custom-file t) ; Tolerate the file not existing.
