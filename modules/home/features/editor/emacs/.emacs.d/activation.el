;;; -*- lexical-binding: t -*-
;:* activation.el
;:*=======================
;;;** Consulting the activation names the "activation-names.el" file lists
(require 'cl-lib)

;; The "activation-names.el" file, which the "init.el" file loads
;; before this one, defines these two lists.
(defvar seh-known-activation-names)
(defvar seh-activation-names-in-effect)

(cl-defun seh-nearest-strings (target candidates &optional (limit 3))
  "Return the CANDIDATES within LIMIT edits of TARGET, nearest first.
LIMIT must be a positive integer."
  (cl-check-type limit (integer 1 *))
  (cl-loop for candidate in candidates
           for distance = (string-distance target candidate)
           when (<= distance limit)
           collect (cons distance candidate) into scored
           finally return (mapcar #'cdr (sort scored :key #'car))))

(cl-defun seh-activation-name-in-effect-p (name &optional (warn t))
  "Return t when the activation name NAME is in effect here, else nil.
A NAME absent from both `seh-activation-names-in-effect' and
`seh-known-activation-names' is most likely misspelled, so warn and
suggest the nearest known names; pass nil as WARN to suppress that."
  (or (and (member name seh-activation-names-in-effect) t)
      (progn
        (when (and warn (not (member name seh-known-activation-names)))
          (let ((nearest (seh-nearest-strings name seh-known-activation-names)))
            (display-warning 'seh-activation
                             (if nearest
                                 (format "No feature or interest is registered under the activation name %S; did you mean %s?"
                                         name (mapconcat (lambda (n) (format "%S" n)) nearest " or "))
                               (format "No feature or interest is registered under the activation name %S" name))
                             :error)))
        nil)))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "Activation settings initialized")
