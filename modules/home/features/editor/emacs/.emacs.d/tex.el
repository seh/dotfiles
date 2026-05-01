;:* tex.el
;:*=======================
;:* Initialize AUC Tex - not the "default" TeX modes
(require 'tex-site)

(defun SEH-TeX-mode-hook ()
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-engine 'xetex)
  (TeX-PDF-mode t)

  ;; (let ((style (assoc "^pdf$" TeX-output-view-style)))
  ;;   (if style
  ;;       (setf (third style)
  ;;             ;; Since it's started by zsh, use Cygwin paths:
  ;;             "/cygdrive/c/Program\\ Files/Adobe/Reader\\ 9.0/Reader/AcroRd32.exe %o"
  ;;             ;; Or just defer to Windows:
  ;;             ;;"cygstart %o"
  ;;             )))
  (turn-on-auto-fill)
  (turn-on-font-lock))

(add-hook 'TeX-mode-hook 'SEH-TeX-mode-hook t)
(add-hook 'TeX-mode-hook 'turn-on-reftex)
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "TeX settings initialized")
