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

(defun seh-unregistered-activation-name-message (name)
  "Return a message saying that no feature or interest registers NAME.
Suggest the nearest of `seh-known-activation-names' when any lie
within a few edits of NAME."
  (let ((nearest (seh-nearest-strings name seh-known-activation-names)))
    (if nearest
        (format "No feature or interest is registered under the activation name %S; did you mean %s?"
                name (mapconcat (lambda (n) (format "%S" n)) nearest " or "))
      (format "No feature or interest is registered under the activation name %S" name))))

(cl-defun seh-activation-name-in-effect-p (name &optional (warn t))
  "Return t when the activation name NAME is in effect here, else nil.
A NAME absent from both `seh-activation-names-in-effect' and
`seh-known-activation-names' is most likely misspelled, so warn and
suggest the nearest known names; pass nil as WARN to suppress that."
  (or (and (member name seh-activation-names-in-effect) t)
      (progn
        (when (and warn (not (member name seh-known-activation-names)))
          (display-warning 'seh-activation
                           (seh-unregistered-activation-name-message name)
                           :error))
        nil)))

;; The byte compiler consults this compiler macro for each call to
;; the predicate above. When the byte compiler is running, NAME is a
;; literal string, WARN is absent, and both lists are bound, the call
;; becomes the constant t or nil, so a guard such as
;; "(when (seh-activation-name-in-effect-p "lang/go") ...)" loses its
;; test and the optimizer drops the branch that cannot run. A literal
;; NAME absent from both lists is an error in the compilation, which
;; fails the build that runs it, in place of the warning the
;; predicate displays at run time. Every other call compiles as an
;; ordinary call to the predicate, and so does a file evaluated by
;; hand, since macroexpansion for evaluation is not the byte
;; compiler; the predicate therefore remains a function.
(declare-function byte-compile-report-error "bytecomp")
(cl-define-compiler-macro seh-activation-name-in-effect-p
    (&whole form name &optional (_warn nil warn-supplied-p))
  (cond ((not (and (macroexp-compiling-p)
                   (stringp name)
                   (not warn-supplied-p)
                   (boundp 'seh-known-activation-names)
                   (boundp 'seh-activation-names-in-effect)))
         form)
        ((member name seh-activation-names-in-effect) t)
        ((member name seh-known-activation-names) nil)
        (t
         (byte-compile-report-error
          (seh-unregistered-activation-name-message name))
         form)))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "Activation settings initialized")
