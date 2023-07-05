;:* sgml.el
;:*=======================
;:* the PSGML package
(require 'psgml)
(setq auto-mode-alist
      (append '(("\\.xsl$" . xml-mode)
                ) auto-mode-alist))


;:* SGML helper functions
(defun sgml-next-trouble-spot-wrap ()
  "Find next trouble spot, wrapping at end of document"
  (interactive)
  (if (= (point) (point-max))
      (goto-char (point-min)))
  (sgml-next-trouble-spot))

; SGML General Entity expansion
(defun sgml-report-entities-expanded (n)
  "Print a report of how many SGML General Entities were replaced"
  (display-message 'command (format "Expanded %i entity reference%s" n (if (not
                                                                            (= 1 n)) "s" ""))))

(defun sgml-find-next-entity-reference ()
  "Find next SGML General Entity reference"
  (interactive)
  (if (search-forward-regexp "&[a-zA-Z][^;]*;" nil t)
      (goto-char (match-beginning 0))))

(defun sgml-expand-next-entity-reference (&optional n)
  "Expand next SGML General Entity reference"
  (interactive "p")
  (let ((cn 0))
    (while (and (< cn n) (sgml-find-next-entity-reference))
      (progn
	(sgml-expand-entity-reference)
	(incf cn)))
    (if cn (sgml-report-entities-expanded cn))))

(defun sgml-expand-all-entity-references ()
  "Expand all SGML General Entity references in the buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((cn 0))
      (while (sgml-find-next-entity-reference)
	(sgml-expand-entity-reference)
	(incf cn))
      (sgml-report-entities-expanded cn))))


;:* SEH-sgml-mode-hook
(defun SEH-sgml-mode-hook ()
  (display-message 'progress "Running SEH-sgml-mode-hook")

  (define-key (current-local-map) 'f6 'sgml-parse-prolog)
  (define-key (current-local-map) 'f7 'sgml-next-trouble-spot-wrap)
  (define-key (current-local-map) 'f8 'sgml-expand-next-entity-reference)


  ; most of these are set by Megginsons's XML package, but we can do it manually here too
  ;(setq sgml-omittag nil)                       ; (OMITTAG forbidden by XML)
  ;(setq sgml-shorttag nil)                      ; (SHORTTAG forbidden by XML)
  ;(setq sgml-minimize-attributes nil)           ; (Forbidden by XML)
  ;(setq sgml-always-quote-attributes t)         ; (Required by XML)
  ;(setq sgml-namecase-general nil)              ; (Required by XML)

  (setq-default sgml-indent-data t)
  (setq sgml-live-element-indicator t)

  (setq sgml-catalog-files (append sgml-catalog-files '("~/doc/DTD/CATALOG")))


  ;; Tag coloring definitions
  (setq sgml-set-face t
        sgml-auto-activate-dtd t)

;  (set-face-foreground 'sgml-comment-face "darkgrey")
;  (set-face-foreground 'sgml-doctype-face "rosybrown")
;  (set-face-foreground 'sgml-tag-face "mediumpurple")
;  (set-face-foreground 'sgml-start-tag-face "mediumorchid")
;  (set-face-foreground 'sgml-end-tag-face "mediumpurple")
;  (set-face-foreground 'sgml-entity-face "indianred")
;  (set-face-foreground 'sgml-ignored-face "maroon")
;  (set-face-background 'sgml-ignored-face "darkgrey")
;  (set-face-foreground 'sgml-ms-end-face "maroon")
;  (set-face-foreground 'sgml-ms-start-face "maroon")
;  (set-face-foreground 'sgml-pi-face "lightseagreen")
;  (set-face-foreground 'sgml-sgml-face "maroon")
;  (set-face-foreground 'sgml-short-ref-face "goldenrod")

  ;; The defaults proposed in psgml.el don't kick in because `facep'
  ;; returns nil against each of these.
  (setq sgml-markup-faces
      '((start-tag . font-lock-function-name-face)
        (end-tag . font-lock-function-name-face)
        (comment . font-lock-comment-face)
        (pi . font-lock-type-face)
        (sgml . font-lock-type-face)
        (doctype . font-lock-keyword-face)
        (entity . font-lock-string-face)
        (shortref . font-lock-string-face)
        (ignored . font-lock-constant-face)
        (ms-start . font-lock-constant-face)
        (ms-end . font-lock-constant-face)))

  (turn-on-font-lock))

(add-hook 'sgml-mode-hook 'SEH-sgml-mode-hook)
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "sgml settings initialized")