;;; -*- lexical-binding: t -*-
;:* lua.el
;:*=======================
;;;** Lua modes

(use-package lua-ts-mode
  :defer t
  :config
  (setq
   ;; This is 4 by default.
   lua-ts-indent-offset 2))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "Lua settings initialized")
