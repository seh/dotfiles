;;; -*- lexical-binding: t -*-
;:* c-and-java.el
;:*=======================
;;;** 'c++-mode' package

(use-package cc-mode
  :defines (whitespace-style)
  :functions (c-forward-conditional
              c-toggle-auto-hungry-state)
  :preface
  (defconst SEH-c-offset 2)

  (defconst SEH-k&r-c-style
    `("k&r"
      (c-hanging-braces-alist         . ((brace-list-open)
                                         (brace-list-close)))
      (c-cleanup-list                 . (scope-operator
                                         empty-defun-braces
                                         defun-close-semi))
      (c-basic-offset                 . ,SEH-c-offset)
      (c-offsets-alist                . ((inline-open . 0)
                                         (arglist-cont-nonempty . c-lineup-arglist)))
      (c-indent-comments-syntactically-p . t))
    "SEH C/C++ Programming Style (K&R)")

  (defconst SEH-whitesmith-c-style
    '("whitesmith"
      (c-basic-offset                 . 3)
      ;; inline-open (default +) seems to overlap with inclass set to
      ;; `c-lineup-whitesmith-in-block', which already takes care of
      ;; indenting an opening brace relative to its containing block:
      (c-offsets-alist                . ((inline-open . 0)
                                         (innamespace . 0)
                                         (case-label . -) ; default 0
                                         (statement-case-open . +) ; default 0
                                         ;; Just adding features to defaults:
                                         (topmost-intro-cont
                                          . (c-lineup-cascaded-calls
                                             c-lineup-topmost-intro-cont))
                                         (statement-cont
                                          . (c-lineup-cascaded-calls
                                             +))
                                         (arglist-cont
                                          . (c-lineup-gcc-asm-reg
                                             c-lineup-cascaded-calls
                                             c-lineup-argcont))
                                         (arglist-cont-nonempty
                                          . (c-lineup-gcc-asm-reg
                                             c-lineup-cascaded-calls
                                             c-lineup-argcont
                                             c-lineup-arglist))))
      (c-cleanup-list                 . (scope-operator
                                         ;; If only we could exclude class-close:
                                         empty-defun-braces
                                         defun-close-semi
                                         list-close-comma))
      )
    "SEH C/C++ Programming Style (Whitesmith)")

  (defconst vertica-c-style
    '("ellemtel"
      (c-basic-offset                 . 4)
      (c-hanging-colons-alist         . ((case-label after)
                                         (label after)
                                         (access-label after)
                                         (member-init-intro before)
                                         (inher-intro)))
      (c-hanging-semi&comma-criteria
       .
       (c-semi&comma-no-newlines-for-oneline-inliners
        c-semi&comma-inside-parenlist
        c-semi&comma-no-newlines-before-nonblanks))
      (c-indent-comments-syntactically-p . t)
      (c-indent-comment-alist . ((other . (space . 2))))
      (c-cleanup-list . (empty-defun-braces
                         defun-close-semi
                         list-close-comma
                         scope-operator))
      ;; TODO: Figure out which of these to adjust.
      (c-offsets-alist . ((statement-cont . (c-lineup-assignments ++))
                          (innamespace . 0)))
      )
    "Vertica C/C++ Programming Style")

  (defun SEH-c-mode-common-hook ()
    (require 'compile)

    (c-toggle-auto-hungry-state t)
    (turn-on-font-lock)
    (setq
     indent-tabs-mode nil                 ; Indentation can insert tabs if this is non-nil.
     tab-width c-basic-offset
     c-echo-syntactic-information-p t
     compilation-ask-about-save nil)
    (define-key (current-local-map) [f7] 'compile)

    (require 'whitespace)
    (setq whitespace-style '(face empty tabs lines-tail trailing))
    (whitespace-mode))

  (defun SEH-c-c++-mode-hook ()
    (c-add-style "SEH-K&R" SEH-k&r-c-style)
    (c-add-style "SEH-Whitesmith" SEH-whitesmith-c-style)
    (c-add-style "Vertica" vertica-c-style t)

    ;; Try to preserve the defaults, and just adjust these few:
    (unless (eq 'set-from-style c-hanging-braces-alist)
      (setq c-hanging-braces-alist
            (cons '(brace-list-close)     ; Why isn't this in there?
                  (assq-delete-all 'substatement-open c-hanging-braces-alist)))))

  (defun make-include-guard-name (basename)
    (replace-regexp-in-string
     "^\\(_+\\)\\(.+\\)" "\\2\\1"
     (upcase (replace-regexp-in-string "[-.]" "_" basename))))

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
      (forward-comment (buffer-size))
      (insert (format "#ifndef %s\n#define %s\n\n" guardname guardname))
      (goto-char (point-max))
      (delete-blank-lines)
      (insert (concat "\n\n#endif	// " guardname))))


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
            (insert guardname)
            (unless quiet
              (message "Matched at line %d." (line-number-at-pos)))))))
  :mode (;; Force `.h' files to be `c++' files rather than straight `c.'
         ("\\.h\\'" . c++-mode)
         ;; Force `.inl' files to be `c++' files.
         ("\\.inl\\'" . c++-mode))
  :hook ((c-mode-common . SEH-c-mode-common-hook)
         (c-mode . SEH-c-c++-mode-hook)
         (c++-mode . SEH-c-c++-mode-hook)))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "C and Java settings initialized")
