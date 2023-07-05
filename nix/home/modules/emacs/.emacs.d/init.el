;* ~/.emacs
;:*=======================


(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defconst user-init-dir
  (cond ((and (not (boundp 'aquamacs-version))
	      ;; Aquamacs uses ~/Library/Preference/Aquamacs Emacs/.
	      (boundp 'user-emacs-directory))
	 user-emacs-directory)
	((boundp 'user-init-file)
	 (file-name-directory user-init-file))
	((boundp 'user-init-directory)
	 user-init-directory)
	(t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

;:*=======================
(load-user-file "packages.el")

;:*=======================
(load-user-file "personal.el")

;:*=======================
(load-user-file "platform.el")

;:*=======================
;(load-user-file "cygwin.el")

;:*=======================
(load-user-file "variables.el")

;:*=======================
(load-user-file "paths.el")

;:*=======================
(load-user-file "mail-news.el")

;:*=======================
(load-user-file "misc-funcs.el")

;:*=======================
(load-user-file "bbdb.el")

;:*=======================
(load-user-file "calendar.el")

;; TODO: gnus-funcs

;:*=======================
(load-user-file "c-and-java.el")

;:*=======================
(load-user-file "go.el")

;:*=======================
(load-user-file "lisp.el")

;:*=======================
;(load-user-file "clojure.el")

;:*=======================
;(load-user-file "sgml-xml.el")

;:*=======================
(load-user-file "markdown.el")

;:*=======================
(load-user-file "rust.el")

;:*=======================
(load-user-file "tex.el")

;:*=======================
(load-user-file "org-config.el")

;:*=======================
(load-user-file "fonts-basic.el")

;:*=======================
;(load-user-file "fonts.el")
 
;:*=======================
;; TODO(seh): Reenable this after we've figured out why the "light" background mode doesn't work.
;(load-user-file "color-theme.el")

;:*=======================
(load-user-file "frame.el")

;:*=======================
(load-user-file "server.el")

;:*=======================
(load-user-file "keys.el")

;:*=======================
(load-user-file "macos.el")

;; TODO: Move this out to packaegs.el or something.
(require 'ehelp)
(define-key global-map "\C-h" 'ehelp-command)
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(sanityinc-tomorrow-blue))
 '(custom-safe-themes
   '("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" default))
 '(package-selected-packages
   '(nix-mode nix-modeline nixpkgs-fmt 0blayout abyss-theme 2048-game lsp-mode lsp-ui flycheck-tip cargo cargo-transient flycheck-rust rust-mode color-theme-sanityinc-tomorrow persistent-scratch yasnippet yaml-mode use-package tide terraform-mode smex slime-repl-ansi-color slime-company org nginx-mode monokai-theme magit-todos magit-filenotify lua-mode jsonnet-mode json-mode js2-mode inf-ruby ido-completing-read+ go-mode exec-path-from-shell elvish-mode dired-subtree company-nginx color-theme-sanityinc-solarized color-theme-modern boxquote bbdb bazel auctex))
 '(server-done-hook (lambda nil (delete-frame)))
 '(server-switch-hook
   (lambda nil
     (let
         ((server-buf
           (current-buffer)))
       (bury-buffer)
       (switch-to-buffer-other-frame server-buf)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color)) (:family "DejaVu Sans Mono"))))
 '(eldoc-highlight-function-argument ((((class color)) (:foreground "tomato" :bold t))))
 '(shell-output-face ((((class color) (background dark)) (:italic nil :foreground "gray90")) (((class color) (background light)) (:italic nil :foreground "darkblue"))) t)
 '(shell-prompt-face ((((class color) (background dark)) (:bold t :foreground "palegreen")) (((class color) (background light)) (:bold t :foreground "mediumslateblue"))) t))
