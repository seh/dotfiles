;:* paths.el
;:*=======================
(defconst SEH-install-prefix
  (expand-file-name "usr/local" "~/")
  "Configured prefix path to my locally-installed executables")

(defconst SEH-site-lisp-dir
  (expand-file-name "share/emacs/site-lisp" SEH-install-prefix)
  "Path to site-lisp directory")

(defconst SEH-site-packages-dir
  SEH-site-lisp-dir
  "Path to site-packages directory")

(add-to-list 'load-path SEH-site-lisp-dir)

;; Experimental
;(add-to-list 'load-path (expand-file-name "gnus" SEH-site-packages-dir))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "path settings initialized")
