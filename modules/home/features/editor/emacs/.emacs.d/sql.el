;;; -*- lexical-binding: t -*-
;:* sql.el
;:*=======================
;;;** SQL modes

(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "SQL settings initialized")
