;:* misc-funcs.el
;:*=======================
(defun copy-current-line ()
  "Copy the current line into the kill buffer."
  (interactive)
  (copy-region-as-kill (point-at-bol) (point-at-eol)))

(defun vi-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun vi-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(defun vi-open-line (&optional abovep)
  "Insert a newline below the current line and put point at beginning.
With a prefix argument, insert a newline above the current line."
  (interactive "P")
  (if abovep
      (vi-open-line-above)
    (vi-open-line-below)))

(defun line-to-top-of-window ()
  "Move current line to top of window"
  (interactive)
  (recenter 0))

(defun other-window-backward (&optional n)
  "Select Nth previous window"
  (interactive "p")
  (other-window (- n)))

(defun forward-line-skipping-blanks (&optional n)
  "Move ARG lines forward (backward if ARG is negative), skipping blank lines along the way."
  (interactive "p")
  (let ((cn 0))
    (while (< cn n)
      (forward-line)
      (while (string-match "^\\s-*$" (buffer-string (point-at-bol)
                                                    (point-at-eol)))
        (forward-line))
      (incf cn))))

(defun font-lock-and-fontify ()
  "Turn on font-lock mode, or call font-lock-fontify-buffer"
  (interactive)
  (if (null font-lock-mode)
      (font-lock-mode t)
    (font-lock-fontify-buffer)))

(defun toggle-truncate-lines ()
  "Toggle word-wrap mode"
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (recenter))

(defun remove-trailing-spaces ()
  "Remove trailing spaces in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((case-fold-search nil))
          (while (re-search-forward "[ \t]+$" (point-max) t)
            (replace-match "")))))))

;; a la Graham's "On Lisp"
(defmacro nonzero-bind (var exp &rest body)
  `(let ((,var ,exp))
     (unless (zerop ,var)
       ,@body)))

(defun underline-current (&optional str)
  "Insert multiple copies of STR under and of same width as current line.

The default underline string is =.

With a prefix argument, prompt for the underline string."
  (interactive (list (if current-prefix-arg
                         (read-string "Underline string: "
                                      "="))))
  (save-excursion
    (end-of-line)
    (nonzero-bind len (current-column)
                  (newline)
                  (let* ((str (or str "="))
                         (slen (length str)))
                    (cond
                      ((zerop slen)
                        (error "Underline string must be at least one character."))
                      ((= 1 slen)
                       (insert-char (aref str 0) len))
                      (t
                       (dotimes (i (/ len slen))
                         (insert str))
                       (nonzero-bind rem (mod len slen)
                                     (insert-string (substring str 0 rem)))))))))


(defun replace-env-var-substr (var-name old-str new-str)
  "Replace old-str with new-str in environment variable var-name"
  (interactive (let (var from to)
		 (setq var (read-string "Variable name: "))
		 (setq from (read-string "Substring to replace: "))
		 (list var from (read-string (format "Replace %s with: " from)))))
  (let ((var-val (getenv var-name)))
    (if (not var-val)
	(message (format "Variable %s's value is nil." var-name))
      (set-buffer (get-buffer-create " *pathbuf*"))
      (insert-string var-val)
      (goto-char (point-min))
      (while (search-forward old-str nil t)
	(replace-match new-str))
      (setenv var-name (buffer-string (point-min) (point-max)))
      (kill-buffer nil))))


;(defconst resolve-rev-var "RESOLVE_REV")


;(defun update-resolve-rev (new-rev &optional old-rev)
;  "Change RESOLVE_REV and PATH variables to new-rev.

;With a prefix argument, prompt for the old revision string as well."
;  (interactive (list (read-string (concat "New " resolve-rev-var ": ") (getenv resolve-rev-var))
;		     (and current-prefix-arg (read-string (concat "Old " resolve-rev-var ": ")
;							  (getenv resolve-rev-var)))))
;  (replace-env-var-substr "PATH" (or old-rev (getenv resolve-rev-var))
;			  new-rev)
;  (setenv resolve-rev-var new-rev))

(defun insert-todo-comment (arg)
  (interactive "*P")
  (comment-dwim arg)
  (insert "TODO(" (user-login-name) "): "))

(defun make-include-guard-name (basename)
  (replace-in-string (upcase (replace-in-string basename "[-.]" "_"))
                     "^\\(_+\\)\\(.+\\)"
                     "\\2\\1"))

(defun default-include-guard-name ()
  (make-include-guard-name (buffer-name)))

(defun insert-include-guards (&optional guardname)
  "Add #include preprocessor guard GUARDNAME to the current buffer.

The default guardname is the current buffer's name, upcased with
hyphens and periods replaced with underscores.

With a prefix argument, prompt for the guard name."
  (interactive (list (if current-prefix-arg
                         (read-string "Guard name: "
                                      (default-include-guard-name)))))
  (if (null guardname)
      (setq guardname (default-include-guard-name)))
  (if (string= guardname "")
      (error "Guard name cannot be nil."))
  (save-excursion
    (goto-char (point-min))
    (while (forward-comment))
    (insert-string (format "#ifndef %s\n#define %s\n\n" guardname guardname))
    (goto-char (point-max))
    (delete-blank-lines)
    (insert-string (concat "\n\n#endif	// " guardname))))


(defun insert-matching-conditional-comment (&optional quiet)
  "Add symmetric comment to preprocessor guard on or after point.

If the optional argument QUIET is non-nil, no messages will be printed."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "^#if\\(\\(n\\)?def\\)?\\s-+\\([A-Za-z0-9_-]+\\)\\s-*$" nil t)
        (let ((guardname
               (buffer-substring (match-beginning 3) (match-end 3))))
          (c-forward-conditional 1 -1)
          (backward-char)
          (indent-for-comment)
          (insert-string guardname)
          (unless quiet
            (message "Matched at line %d." (line-number)))))))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "miscellaneous functions initialized")
