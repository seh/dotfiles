;:* fonts.el
;:*
;:* Much of this file is borrowed from Robin S. Socha <robin@socha.net>:
;:* http://xemacs.socha.net/fonts-rss.el.html
;:*=======================

(set-cursor-color "pale goldenrod")

;:*======================
;:* loads of faces...
(custom-set-faces
 ;;:* normal text
 '(default
   ((((class color))
     (:family "Monaspace Neon"
      :foreground "gray90" :background "black"
      ;; For the "default" face, we must use a fixed value. We cannot
      ;; compute it like this:
      ;;  (floor (face-attribute 'default :height)
      ;;    text-scale-mode-step)
      ;:height 114
      ))) t)

 ;;:* selecting w/ mouse
 '(region
   ((((class color) (background dark))
     (:foreground "yellow" :background "gray30"))) t) ; or "chartreuse" fg.

 ;;:* selecting buffers
 '(highlight
   ((((class color) (background dark))
     (:foreground "yellow" :background "mediumpurple3"))) t)

 ;;:* highlighting while searching
 '(isearch
   ((((class color) (background dark))
     (:foreground "red" :background "yellow"))) t)

 '(isearch-secondary
   ((((class color) (background dark))
     (:foreground "yellow"))) t)

 ;;:* modeline
 '(modeline
   ((((class color) (background dark))
     (:foreground "black" :background "gray75"))) t)

 ;;:* mouse pointer
 '(pointer
   ((((class color) (background dark))
     (:foreground "green3"))) t)

 ;;:* cursor
 '(text-cursor
   ((((class color) (background dark))
     ;; Was "paleyellow":
     (:background "#ffffcc"))) t) ; or "tomato" or "orangered"

 ;;:* matching parens
 '(show-paren-match
   ((((class color) (background dark))
     ;; Was "paleyellow":
     (:foreground "#ffffcc" :background "mediumpurple"))) t)
 ;; or "red", was "black" for fg.
 ;; maybe "seagreen3" or "darkorange3", was "darkseagreen2" for bg.
 )


;:* hyper-apropos
(eval-after-load "hyper-apropos"
  '(progn
     (custom-set-faces
      '(hyper-apropos-hyperlink
        ((((class color) (background dark))
          (:foreground "darkseagreen2"))) t) ; was "blue4"

      '(hyper-apropos-documentation
        ((((class color) (background dark))
          (:foreground "orangered"))) t) ; was "darkred"
      )))

;:* info-mode
(eval-after-load "info"
  '(progn
     (custom-set-faces
      '(info-node
        ((((class color) (background dark))
          (:foreground "slateblue"))) t)

      '(info-xref
        ((((class color) (background dark))
          (:foreground "orangered"))) t)
      )))

;:* bbdb
(eval-after-load "bbdb-gui"
  '(progn
     (custom-set-faces
      '(bbdb-name
        ((((class color) (background dark))
          (:foreground "palegreen"))) t)

      '(bbdb-company
        ((((class color) (background dark))
          (:foreground "lightslategrey"))) t)

;      '(bbdb-field-name
;        ((((class color) (background dark))
;          (:foreground ""))) t)

;      '(bbdb-field-value
;        ((((class color) (background dark))
;          (:foreground ""))) t)
     )))

;:* calendar
(eval-after-load "calendar"
  '(progn
     (custom-set-faces
      '(calendar-today-face
        ((((class color) (background dark))
          (:foreground "Tomato" :bold t :underline t))) t)

      '(diary-face
        ((((class color) (background dark))
          (:foreground "PaleGreen" :bold t))) t)

      '(holiday-face
        ((((class color) (background dark))
          (:background "DarkSlateBlue"))) t)
    )))

;:* diff-mode
(eval-after-load "diff-mode"
  '(progn
     (custom-set-faces
      '(diff-added-face
        ((((class color) (background dark))
          (:foreground "palegreen"))) t)

      '(diff-removed-face
        ((((class color) (background dark))
          (:foreground "lightslategrey"))) t)

      '(diff-changed-face
        ((((class color) (background dark))
          (:foreground "yellow"))) t)

;      '(diff-index-face
;        ((((class color) (background dark))
;          (:foreground "" :bold t))) t)

      '(diff-file-header-face
        ((((class color) (background dark))
          (:foreground "darkviolet" :background "grey70" :bold t))) t)

      '(diff-hunk-header-face
        ((((class color) (background dark))
          (:foreground "palegoldenrod" :background "gray30" :bold t))) t)
      )))

;:* font-lock-faces
(eval-after-load "font-lock"
  '(progn
     (custom-set-faces
      '(font-lock-comment-face
        ((((class color) (background dark))
          (:foreground "indianred"))) t)

      '(font-lock-doc-string-face
        ((((class color) (background dark))
          (:foreground "red"))) t)

      '(font-lock-function-name-face
        ((((class color) (background dark))
          (:foreground "aquamarine" :bold t))) t)

;      '(font-lock-keyword-face
;        ((((class color) (background dark))
;          (:foreground "red"))) t) ; maybe the default is fine

;      '(font-lock-preprocessor-face
;        ((((class color) (background dark))
;          (:foreground "magenta3"))) t) ; maybe the default is fine

      '(font-lock-reference-face
        ((((class color) (background dark))
          (:foreground "steelblue"))) t) ; was "cadetblue2"

      '(font-lock-string-face
        ((((class color) (background dark))
          (:foreground "tomato" :bold t))) t) ; was "tan"

;      '(font-lock-type-face
;        ((((class color) (background dark))
;          (:foreground "dodgerblue"))) t) ; was "wheat"

      '(font-lock-variable-name-face
        ((((class color) (background dark))
          (:foreground "lightslateblue"))) t) ; was "cyan"
      )))

(eval-after-load "message"
  '(progn
     ;; TODO: These are duplicated in alternate form for experimentation...
     (custom-set-faces
      ;; Face used for displaying header names.
      '(message-header-name
        ((((class color) (background dark))
          (:foreground "lightsteelblue"))) t) ; was "DarkGreen"

      ;; Face used for displaying To headers.
      '(message-header-to
        ((((class color) (background dark))
          (:foreground "palegreen" :bold t))) t) ; was "green2"

      ;; Face used for displaying Cc headers.
      '(message-header-cc
        ((((class color) (background dark))
          (:foreground "steelblue2" :bold t))) t) ; was "green4"

      ;; Face used for displaying subject headers.
      '(message-header-subject
        ((((class color) (background dark))
          (:foreground "sandybrown"))) t) ; was "green3"

     ;; Face used for displaying newsgroups headers.
;      '(message-header-newsgroups
;        ((((class color) (background dark))
;          (:foreground "yellow"))) t) ; was "yellow"

      ;; Face used for displaying X-* headers.
;      '(message-header-xheader
;        ((((class color) (background dark))
;          (:foreground "blue"))) t) ; was "blue"

      ;; Face used for displaying other headers.
      '(message-header-other
        ((((class color) (background dark))
          (:foreground "dodgerblue"))) t) ; was "#b0000"

      ;; Face used for displaying the separator.
      '(message-separator
        ((((class color) (background dark))
          (:foreground "cornflowerblue"))) t) ; was "blue3"

      ;; Face used for displaying cited text.
      '(message-cited-text
        ((((class color) (background dark))
          (:foreground "gray60"))) t) ; was "red"

      ;; Face used for displaying MML.
;      '(message-mml
;        ((((class color) (background dark))
;          (:foreground "ForestGreen"))) t) ; was "ForestGreen"
      )))

(eval-after-load "font-latex"
  '(progn
     (custom-set-faces
      '(font-latex-sedate-face
        ((((class color) (background dark))
          (:foreground "Turquoise"))) t) ; was "LightGray"
      )))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "font settings initialized")
