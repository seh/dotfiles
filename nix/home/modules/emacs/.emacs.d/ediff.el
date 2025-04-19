;:* ediff.el
;:*=======================
;; See the `ediff-toggle-show-clashes-only' function, bound to `$$' in
;; ediff's "control buffer".
(setq ediff-show-clashes-only t
      ediff-keep-variants nil
      ediff-autostore-merges nil)

(defun seh-ediff-janitor ()
  (ediff-janitor nil nil))

(add-hook 'ediff-cleanup-hook #'seh-ediff-janitor)

;; These new few forms are for using "ediff" as a merge tool in
;; concert with the "jujutsu" tool.

(defvar *jj-ediff-merge-quit-sentinel-file* nil)

(defun seh-jj-resolve-ediff-quit-merge-hook ()
  (when-let ((f *jj-ediff-merge-quit-sentinel-file*))
    (delete-file f)))

(add-hook 'ediff-quit-merge-hook #'seh-jj-resolve-ediff-quit-merge-hook 99)
;; TODO(seh): Is this one not already registered by default?
(add-hook 'ediff-quit-merge-hook #'ediff-maybe-save-and-delete-merge)

;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "ediff settings initialized")
