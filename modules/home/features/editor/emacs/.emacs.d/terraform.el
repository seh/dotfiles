;;; -*- lexical-binding: t -*-
;:* terraform.el
;:*=======================
(use-package terraform-mode
  :defines (terraform-format-on-save)
  :hook (terraform-mode . outline-minor-mode)
  :config
  (setq
   terraform-format-on-save t))
;:::::::::::::::::::::::::::::::::::::::::::::::::*
(message "Terraform settings initialized")
