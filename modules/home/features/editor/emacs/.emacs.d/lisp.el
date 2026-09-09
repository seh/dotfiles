;;; -*- lexical-binding: t -*-
;:* lisp.el
;:*=======================
(declare-function seh-activation-name-in-effect-p "activation")

(defmacro make-key-inserter (def)
  "Substitute for `self-insert-command'."
  `(lambda (arg)
     (interactive "*P")
     (insert-char ,def (prefix-numeric-value arg))))

(define-key lisp-mode-shared-map "[" 'insert-parentheses)
(define-key lisp-mode-shared-map "]" 'move-past-close-and-reindent)
(define-key lisp-mode-shared-map "\M-[" (make-key-inserter ?\[))
(define-key lisp-mode-shared-map "\M-]" (make-key-inserter ?\]))


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
