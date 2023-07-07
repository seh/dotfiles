;;; cue-mode.el --- Major mode for editing CUE files

(defgroup cue '()
  "Major mode for editing CUE files."
  :group 'languages)

(defcustom cue-command
  "cue"
  "CUE command to run in ‘cue-eval-buffer’ and ‘cue-reformat-buffer’."
  :type '(string)
  :group 'cue)

(defconst cue--identifier-regexp
  "\\(?:[a-zA-Z][a-zA-Z_$0-9]*\\|[_$][a-zA-Z_$0-9]+\\)"
  "Regular expression matching a CUE identifier.")

(defconst cue--function-name-regexp
  (concat "\\(\\(?:" cue--identifier-regexp ".\\)?" cue--identifier-regexp "\\)\\s-*("))

(defconst cue-font-lock-keywords-1
  (let ((builtin-regex (regexp-opt '("div"
                                     "mod"
                                     "quo"
                                     "rem") 'words))
        (constant-regex (regexp-opt '("false"
                                      "null"
                                      "true") 'words))
        (keyword-regex (regexp-opt '("for"
                                     "if"
                                     "import"
                                     "in"
                                     "let"
                                     "package") 'words))
        (type-regex (regexp-opt '("bool"
                                  "bytes"
                                  "float"
                                  "float32"
                                  "float64"
                                  "int"
                                  "int128"
                                  "int16"
                                  "int32"
                                  "int64"
                                  "int8"
                                  "number"
                                  "rune"
                                  "string"
                                  "uint"
                                  "uint128"
                                  "uint16"
                                  "uint32"
                                  "uint64"
                                  "uint8") 'words))
        (function-name-regex cue--function-name-regexp)
        (definition-name-regex (concat "\\_<\\(_?#" cue--identifier-regexp "\\)\\??\\s-*:"))
        (field-name-regex (concat "\\(?:^\\|[^#]\\)\\_<\\(" cue--identifier-regexp "\\)\\??\\s-*:"))
        (let-declaration-name-regex (concat "\\_<let\\s-+\\(" cue--identifier-regexp "\\)\\s-*="))
        (standard-functions-regex (regexp-opt '("and"
                                                "close"
                                                "len"
                                                ;; NB: The specification claims these exist.
                                                "open"
                                                "or"
                                                "required") 'words)))
    (list
     `(,builtin-regex . font-lock-builtin-face)
     `(,constant-regex . font-lock-constant-face)
     `(,keyword-regex . (1 font-lock-keyword-face))
     `(,type-regex . (1 font-lock-type-face))
     `(,field-name-regex . (1 font-lock-variable-name-face))
     `(,function-name-regex . (1 font-lock-function-name-face))
     `(,definition-name-regex . (1 font-lock-constant-face))
     `(,let-declaration-name-regex . (1 font-lock-variable-name-face))
     '("\\(-?[[:digit:]]+\\(?:\\.[[:digit:]]+\\)?\\)" . font-lock-constant-face)
     `(,standard-functions-regex . font-lock-function-name-face)
     ))
  "Minimal highlighting for ‘cue-mode’.")

(defvar cue-font-lock-keywords cue-font-lock-keywords-1
  "Default highlighting expressions for cue-mode.")

(defun cue--font-lock-open-multiline-string (start)
  "Set syntax of cue multiline \"\"\"...\"\"\" opening delimiter.
START is the position of \"\"\".
Moves point to the first character following open delimiter."
  (let* ((ppss (save-excursion (syntax-ppss start)))
         (in-string (nth 3 ppss))
         (in-comment (nth 4 ppss)))
    (unless (or in-string in-comment)
      (let ((prefix (cue--find-multiline-string-prefix start)))
        (put-text-property start (+ 3 start) 'cue-multiline-string-prefix prefix)
        ;; Tell jit-lock to refontify if this block is modified.
        (put-text-property start (point) 'syntax-multiline t)
        (goto-char (+ 3 start))
        cue-multiline-string-syntax))))

(defun cue--find-multiline-string-prefix (start)
  "Find prefix for multiline \"\"\"...\"\"\" string starting at START.
Moves point to first non-prefix character."
  (goto-char start)
  (end-of-line)
  (while (and (eolp) (not (eobp))) ; skip blank lines
    (goto-char (1+ (point)))
    (re-search-forward "^[[:space:]]*" nil 'move))
  (let ((prefix (match-string 0)))
    (if (looking-at "\\(?:\"\\{3\\}\\|'\\{3\\}\\)")
        ;; Found end delimiter already (multline string that only
        ;; contains blank lines). Make up a prefix that won't exclude
        ;; end delimiter.
        (concat prefix " ")
      prefix)))

(defun cue--font-lock-close-multiline-string (prefix start)
  "Set syntax of cue multiline \"\"\"...\"\"\" closing delimiter.
START is the position of \"\"\".  PREFIX is the (whitespace) preceding \"\"\"."
  (let* ((ppss (syntax-ppss))
         (in-string (nth 3 ppss))
         (string-start (nth 8 ppss)))
    (when in-string
      (let ((ignored-prefix (get-text-property string-start 'cue-multiline-string-prefix)))
        (if (and ignored-prefix
                 (not (string-prefix-p ignored-prefix prefix)))
            cue-multiline-string-syntax)))))

(defun cue--syntax-propertize-function (start end)
  (goto-char start)
  (funcall
   (syntax-propertize-rules
    ("\\(\"\\{3\\}\\|'\\{3\\}\\)\n"
     (1 (cue--font-lock-open-multiline-string (match-beginning 1))))
    ("^\\([[:space:]]*\\)\\(\"\\{3\\}\\|'\\{3\\}\\)"
     (2 (cue--font-lock-close-multiline-string
         (match-string 1) (match-beginning 2)))))
   (point) end))

(defconst cue-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 12" table)
    (modify-syntax-entry ?\n ">" table)
    ;; CUE affords two different quote forms.
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    ;; Definitions start with '#'.
    (modify-syntax-entry ?# "_" table)
    ;; Identifiers can contain the dollar sign.
    (modify-syntax-entry ?$ "_" table)
    ;; Our parenthesis, braces and brackets
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table)
  "Syntax table for `cue-mode'.")

;;;###autoload
(define-derived-mode cue-mode prog-mode "CUE"
  "cue-mode is a major mode for editing CUE files."
  :syntax-table cue-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults) '(cue-font-lock-keywords ;; keywords
                                                   nil                    ;; keywords-only
                                                   nil                    ;; case-fold
                                                   nil                    ;; syntax-alist
                                                   nil                    ;; syntax-begin
                                                   ))
  ;; (set (make-local-variable 'indent-line-function) 'cue-indent)
  (set (make-local-variable 'comment-start) "//")
  (setq-local syntax-propertize-function #'cue--syntax-propertize-function)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-multiline 'append 'local)
  (set (make-local-variable 'fill-column) 100))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.cue\\'" 'cue-mode))

;;;###autoload
(defun cue-reformat-buffer ()
  "Reformat entire buffer using the CUE format utility."
  (interactive)
  (let ((point (point))
        (file-name (buffer-file-name))
        (stdout-buffer (get-buffer-create "*cue fmt stdout*"))
        (stderr-buffer-name "*cue fmt stderr*")
        (stderr-file (make-temp-file "cue-fmt")))
    (when-let ((stderr-window (get-buffer-window stderr-buffer-name t)))
      (quit-window nil stderr-window))
    (unwind-protect
        (let* ((only-test buffer-read-only)
               (exit-code (apply #'call-process-region nil nil cue-command
                                 nil (list stdout-buffer stderr-file) nil
                                 (append '("fmt")
                                         ;; See https://github.com/cuelang/cue/issues/363.
                                         ;;(when only-test '("--test"))
                                         '("-")))))
          (cond ((zerop exit-code)
                 (progn
                   (if (or
                        ;; See https://github.com/cuelang/cue/issues/363.
                        ;;only-test
                        (zerop (compare-buffer-substrings nil nil nil stdout-buffer nil nil)))
                       (message "No format change necessary.")
                     (let ((start (window-start)))
                       (erase-buffer)
                       (insert-buffer-substring stdout-buffer)
                       (goto-char point)
                       (set-window-start (selected-window) start)))
                   (kill-buffer stdout-buffer)))
                ;; See https://github.com/cuelang/cue/issues/363.
                ;;((and only-test (= exit-code 2))
                ;; (message "Format change is necessary, but buffer is read-only."))
                (t (with-current-buffer (get-buffer-create stderr-buffer-name)
                     (setq buffer-read-only nil)
                     (insert-file-contents stderr-file t nil nil t)
                     (goto-char (point-min))
                     (when file-name
                       (while (re-search-forward "^\\(\\s-+\\)-\\(:[[:digit:]]\\)" nil t)
                         (replace-match (concat "\\1" file-name "\\2"))))
                     (set-buffer-modified-p nil)
                     (compilation-mode nil)
                     (display-buffer (current-buffer)
                                     '((display-buffer-reuse-window
                                        display-buffer-at-bottom
                                        display-buffer-pop-up-frame)
                                       .
                                       ((window-height . fit-window-to-buffer))))))))
      (delete-file stderr-file))))

(define-key cue-mode-map (kbd "C-c C-r") 'cue-reformat-buffer)

(provide 'cue-mode)
;;; cue-mode.el ends here

;; TODO(seh): Remove this.
(setq cue-font-lock-keywords cue-font-lock-keywords-1)
