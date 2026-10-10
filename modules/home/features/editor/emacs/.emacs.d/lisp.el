;;; -*- lexical-binding: t -*-
;:* lisp.el
;:*=======================
(require 'cl-lib)

(declare-function seh-activation-name-in-effect-p "activation")

(defun make-key-inserter (character)
  "Return a command that inserts CHARACTER."
  (lambda (arg)
    (interactive "*P")
    (insert-char character (prefix-numeric-value arg))))

(defun seh-binding-active-when (predicate command)
  "Return a binding for COMMAND, conditional on PREDICATE.
Emacs evaluates this binding whenever it resolves the key sequence
bound to it, calling PREDICATE with no arguments. The key sequence is
bound where PREDICATE returns non-nil and unbound elsewhere, which
lets one Emacs bind it in a graphical frame and leave it unbound in a
terminal frame. The `menu-item' form below is the only keymap entry
that Emacs evaluates on lookup; no menu is involved."
  `(menu-item "" ,command
              :filter ,(lambda (binding)
                         (and (funcall predicate) binding))))

(define-key lisp-mode-shared-map "[" 'insert-parentheses)
(define-key lisp-mode-shared-map "]" 'move-past-close-and-reindent)
;; A terminal sends escape sequences back to the program running in
;; it: mouse reports, answers to queries, and—in kitty's protocol—the
;; keys themselves. Each begins with the "ESC [" or "ESC ]" sequence,
;; which Emacs reads as the "M-[" or "M-]" key. A binding for either
;; key stops Emacs from reading the rest of the sequence, so leave
;; both keys unbound in a terminal frame and bind the "C-c [" and
;; "C-c ]" key sequences there to insert literal brackets instead.
(cl-loop with target-keymap = lisp-mode-shared-map
         with predicate = #'display-graphic-p
         with negation = (lambda () (not (funcall predicate)))
         for character in '(?\[ ?\])
         do (cl-loop
             for key-sequence in (list (kbd (format "M-%c" character))
                                       (kbd (format "C-c %c" character)))
             for applies-p in (list predicate negation)
             do (define-key target-keymap key-sequence
                  (seh-binding-active-when applies-p
                                           (make-key-inserter character)))))


(add-hook 'lisp-mode-hook (lambda ()
                            (turn-on-font-lock)))
(when (seh-activation-name-in-effect-p "lang/common-lisp")
  ;(add-to-list 'load-path "/usr/local/lib/common-lisp/slime")
  (use-package slime
    :functions (inferior-slime-mode slime-setup)
    :hook (lisp-mode . slime-lisp-mode-hook)
    :config
    (slime-setup '(slime-fancy
                   ;; Not provided by "fancy":
                   inferior-slime
                   slime-asdf
                   slime-banner
                   ;; This one doesn't `provide' properly:
                   ;; slime-cl-indent
                   slime-xref-browser))

    (add-hook 'inferior-lisp-mode-hook
              (lambda () (inferior-slime-mode t)))
    ;; Enhance completion.
    (let ((sym 'slime-fuzzy-complete-symbol))
      (when (fboundp sym)
        (setq slime-fuzzy-default-completion-ui t)
        (add-to-list 'slime-completion-at-point-functions sym)))
    (define-key lisp-mode-shared-map "\M-\C-]" 'slime-close-all-parens-in-sexp)
    (add-hook 'slime-mode-hook
              (lambda ()
                (let ((sym 'common-lisp-indent-function))
                  (when (fboundp sym)
                    (setq lisp-indent-function sym)))))
    (setq slime-lisp-implementations
          '((sbcl ("sbcl"))))

    (setq common-lisp-hyperspec-root
          "file:/usr/share/doc/hyperspec/")))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "Lisp settings initialized")
