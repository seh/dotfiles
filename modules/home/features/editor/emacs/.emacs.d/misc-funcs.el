;;; -*- lexical-binding: t -*-
;:* misc-funcs.el
;:*=======================
(defun copy-current-line ()
  "Copy the current line into the kill buffer."
  (interactive)
  (copy-region-as-kill (line-beginning-position) (line-end-position)))

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

(defun forward-line-skipping-blanks (&optional n)
  "Move N lines forward, or backward if N is negative.
Blank lines passed along the way do not count toward N."
  (interactive "p")
  (let ((cn 0))
    (while (< cn n)
      (forward-line)
      (while (and (not (eobp))
                  (looking-at-p "[[:space:]]*$"))
        (forward-line))
      (incf cn))))

(defun font-lock-and-fontify ()
  "Turn on Font Lock mode, or refontify the buffer if it is already on."
  (interactive)
  (if (null font-lock-mode)
      (font-lock-mode t)
    (font-lock-ensure)))

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
                       (dotimes (_ (/ len slen))
                         (insert str))
                       (nonzero-bind rem (mod len slen)
                                     (insert (substring str 0 rem)))))))))


(defun replace-env-var-substr (var-name old-str new-str)
  "Replace OLD-STR with NEW-STR in the environment variable VAR-NAME."
  (interactive (let* ((var (read-string "Variable name: "))
                      (from (read-string "Substring to replace: ")))
                 (list var from
                       (read-string (format "Replace %s with: " from)))))
  (let ((var-val (getenv var-name)))
    (if var-val
        (setenv var-name (string-replace old-str new-str var-val))
      (message "Variable %s's value is nil." var-name))))


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

;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "miscellaneous functions initialized")
