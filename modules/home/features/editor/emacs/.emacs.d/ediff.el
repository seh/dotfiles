;;; -*- lexical-binding: t -*-
;:* ediff.el
;:*=======================
(declare-function seh-activation-name-in-effect-p "activation")

(use-package ediff
  :functions (ediff-janitor)
  :preface
  (defun seh-ediff-janitor ()
    (ediff-janitor nil nil))
  :hook ((ediff-cleanup . seh-ediff-janitor)
         ;; TODO(seh): Is this one not already registered by default?
         (ediff-quit-merge . ediff-maybe-save-and-delete-merge))
  :config
  ;; See the `ediff-toggle-show-clashes-only' function, bound to
  ;; `$$' in ediff's "control buffer".
  (setq ediff-show-clashes-only t
        ediff-keep-variants nil
        ediff-autostore-merges nil))

;; These new few forms are for using "ediff" as a merge tool in
;; concert with the "jujutsu" tool.

(defvar *jj-ediff-merge-quit-sentinel-file* nil)

(defun seh-jj-resolve-ediff-quit-merge-hook ()
  (when-let* ((f *jj-ediff-merge-quit-sentinel-file*))
    (delete-file f)))

(when (seh-activation-name-in-effect-p "vcs/jujutsu")
  (add-hook 'ediff-quit-merge-hook #'seh-jj-resolve-ediff-quit-merge-hook 99))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "ediff settings initialized")
