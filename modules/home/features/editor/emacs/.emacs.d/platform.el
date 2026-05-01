;:* platform.el
;:*=======================
(defvar running-on-cygwin-p (memq system-type '(cygwin cygwin32)))
(defvar running-on-mswindows-p (memq system-type
                                     '(windows-nt ms-windows cygwin cygwin32)))

(defun translate-path-canonical (path)
  (if (not running-on-mswindows-p)
      path
    (if running-on-cygwin-p
	(substring (shell-command-to-string (concat "cygpath -u '" path "'")) 0 -1)
      (subst-char-in-string ?\\ ?/ path))))

(defun translate-path-native (path)
  (if (not running-on-mswindows-p)
      path
    (if running-on-cygwin-p
	(substring (shell-command-to-string (concat "cygpath -w " path)) 0 -1)
      (subst-char-in-string ?/ ?\\ path))))


(when running-on-mswindows-p
  (setq w32-pass-alt-to-system t)
  (let ((domain-name-suffix ".sehlabs.com"))
    (unless (or running-on-cygwin-p
                (string-match (regexp-quote domain-name-suffix) system-name))
      (setq system-name
            (concat (downcase system-name) domain-name-suffix)))))


(eval-after-load "nnheader"
  ;; Lifted from nnheader.el:
  '(setq nnheader-file-name-translation-alist
         (mapcar (lambda (c) (cons c ?_))
                 '(?: ?* ?\" ?< ?> ??))))


;; Among other things, this helps emacs-w3m find the w3m binary:
;(add-to-list 'exec-path "/opt/local/bin")
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "platform-specific settings initialized")
